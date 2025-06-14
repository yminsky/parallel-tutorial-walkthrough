# Setting up Dune, picking libraries

I think it would be nice if the tutorial encouraged you to set up dune
in the right way.

- Notably, it's not clear which dune packages you actually need.
  One thing I always find confusing about open-source ocaml is...how
  do you search over the library names?  opam show doesn't tell you,
  what does?

- In the end, I realized I needed to add "parallel" (that was easy)
  and later, that I needed to add parallel.scheduler.work_stealing,
  which I guessed by looking at the structure of my ~/.opam
  directory.  But that's really not ideal.

Maybe we should point to a repo with all the code running properly,
so folk can check it out to resolve such issues? This could be
useful internally too.

Maybe we should encourage Base? I guess that would annoy upstream
compiler devs? But internally, the Base/Core examples will just make a
lot more sense. I kinda want to recommend...what we actually
recommend.

Also, why are we acting as if constructor-based disambiguation doesn't
exist?

# I'm pretty confused about contention

I tried this implementation, and the inner calls to total_and_count
are marked as contended. Not sure why!

```ocaml
let average_par par tree =
  let rec total_and_count par tree =
    match (tree : _ Tree.t) with
    | Leaf x -> ~total:x, ~count:1
    | Node (l, r) ->
      let (~total:total_l, ~count:count_l), (~total:total_r, ~count:count_r) =
        Parallel.fork_join2
          par
          (fun par -> total_and_count par l)
          (fun par -> total_and_count par r)
      in
      let total = total_l +. total_r in
      let count = count_l + count_r in
      ~total, ~count
  in
  let ~total, ~count = total_and_count par tree in
  total /. Float.of_int count
;;
```

Oh, that's very confusing. The issue is that you need the damn type
annotation (tha the return value is a pair of int and float), I guess
so that mode-crossing can prove that the result is uncontended.  The
error message here is not helpful.

# modalities

I know this term has been around for quite a while at this point, but
I still don't get it. I would have thought the thing we're calling a
"modality" is more like a "mode shift", "mode specification", "mode
change"? It's a place where you're specifying the mode within a given
structured value. "modality" just doesn't light up any of the right
neurons in my brain.

# Legacy modes

Looking at the mode intro

I kinda think this is a bad name. Are we planning on changing this to
something else? If not, shouldn't we just call these the default
modes?  That's basically what the mode intro says anyway.

# Mode crossing

This is from the tutorial, and...is kind of a lie, right?

> Locality is irrelevant for types that never cause allocation on the
> OCaml heap, like int. Values of such types mode cross on the
> locality axis; they may be used as global even when they are local.

We think locality is relevant for values that are meant to act as
capabilities.  It only makes sense to say this is true if you only
want to use locality for stack allocation, which just isn't true.  I
wonder if there's a truer thing we can say here that is still pretty
simple.

# Are we going to have a kind for function-i-ness?

Not super relevant, but a question that came to mind for me as I was
reading the mode guide.

# The syntax section includes new modes!

yielding and unyielding, for example, show up in the syntax section,
despite there being no mention of them in the intro. I'd mention in
the intro that the things listed here are not actually the full set of
supported modes.

# Bad formatting in the mode-syntax guide

At the bottom:

```
+—————+————–+ | this | implies this | +—————+————–+ | global | unyielding | +—————+————–+ | local | yielding | +—————+————–+ | stateless | portable | +—————+————–+ | immutable | contended | +—————+————–+ | read | shared | +—————+————–+
```

# Trying to split an rng

I tried to write a parallel algorithm for generating a random tree. It
didn't go great.

```ocaml
let rec random_tree_par par rng size : _ Tree.t =
  if size = 1
  then Leaf (Splittable_random.float rng ~lo:0.0 ~hi:1.0)
  else (
    let left_size = Splittable_random.int rng ~lo:1 ~hi:(size - 1) in
    let right_size = size - left_size in
    let rng' = Splittable_random.split rng in
    let left, right =
      Parallel.fork_join2
        par
        (fun par -> random_tree_par par rng left_size)
        (fun par -> random_tree_par par rng' right_size)
    in
    Node (left, right))
;;
```

Specifically, it can't tell that each rng goes to just one of the
functions, and is never again used in the parent, so in reality,
things are fine. But...how do I prove that to the type system!?!

Separately, it is hard to know why your thing isn't portable.

# Fixing the abstractino problem with Thing

That's actually kinda great. It's still a bit tricky to know how to
track-things down, though.

The first thing I tracked down by annotating the [total_and_count]
function as portable. That pointed straight to Thing.price needing to
be portable, so I fixed that. It was a little harder to note that
Thing.price needed to accept a contended Thing.t.  I wonder how hard
it is going to be for people to track these things down in practice.

# Is Merlin busted?

I noticed it flagging some fake syntax errors. Notably this one:

```
module Rng : Rng = struct
  type t = int ref

  let create x = ref x

  let int t =
    let draw = t.contents in
    t.contents <- t.contents + 1;
    draw
  ;;

  let split t = { contents = t.contents }
end
```

It thinks the keyword struct is a syntax error.

# Confusing type errors with things and atomics

I had this bit of code:

```ocaml
module Thing = struct
  type t : immutable_data =
    { price : float
    ; mood : Mood.t Atomic.t
    }

  let create ~price ~mood = { price; mood = Atomic.make mood }
  let price t = t.price
  let mood t = Atomic.get t.mood
  let cheer_up t = Atomic.set t.mood Happy
  let bum_out t = Atomic.set t.mood Sad
end
```

And it really doesn't like the immutable_data kind there, despite the
tutorial recommending it!

```
File "thing_tree_atomic.ml", lines 12-15, characters 2-5:
12 | ..type t : immutable_data =
13 |     { price : float
14 |     ; mood : Mood.t Atomic.t
15 |     }
Error: The kind of type "t" is immutable_data with Mood.t Core.Atomic.t
         because it's a boxed record type.
       But the kind of type "t" must be a subkind of immutable_data
         because of the annotation on the declaration of the type t.
```

I think this is probably a bug in the tutorial (since just copying the
code in generates the error), but I find the error message above
inscrutable.

Is "immutable_data with Mood.t Core.Atomic.t" a different kind than
immutable_data? What does the "with" syntax do?  Maybe we should bound
them together to make it clearer? i.e.:

    the kind of type "t" is "immutable_data with Mood.t Core.Atomic.t"

# Capsules, expert?

Why are we using the expert interface? Are we experts already?

Also, too much magic. I shouldn't have to guess that I need this line!

    module Capsule = Portable.Capsule.Expert
