function ListReverse<T>(xs: seq<T>): seq<T>
  ensures |ListReverse(xs)| == |xs|
  ensures forall item | item in xs + ListReverse(xs) :: item in ListReverse(xs) <==> item in xs
  decreases |xs|
{
  if |xs| == 0 then []
  else assert xs == [xs[0]] + xs[1..]; ListReverse(xs[1..]) + [xs[0]]
}

// Checked sequence algebra, with explicit structural descent and no axioms.
lemma {:induction false} ListReverseAppend<T>(xs: seq<T>, ys: seq<T>)
  ensures ListReverse(xs + ys) == ListReverse(ys) + ListReverse(xs)
  decreases |xs|
{
  if |xs| > 0 {
    assert xs == [xs[0]] + xs[1..];
    assert xs + ys == [xs[0]] + (xs[1..] + ys);
    assert (xs + ys)[1..] == xs[1..] + ys;
    assert (xs + ys)[0] == xs[0];
    assert ListReverse(xs + ys) == ListReverse(xs[1..] + ys) + [xs[0]];
    ListReverseAppend(xs[1..], ys);
    assert ListReverse(xs) == ListReverse(xs[1..]) + [xs[0]];
    assert (ListReverse(ys) + ListReverse(xs[1..])) + [xs[0]] == ListReverse(ys) + (ListReverse(xs[1..]) + [xs[0]]);
  } else {
    assert xs == [];
    assert xs + ys == ys;
    assert ListReverse(xs) == [];
    assert ListReverse(ys) + [] == ListReverse(ys);
  }
}

lemma {:induction false} ListReverseInvolution<T>(xs: seq<T>)
  ensures ListReverse(ListReverse(xs)) == xs
  decreases |xs|
{
  if |xs| > 0 {
    assert xs == [xs[0]] + xs[1..];
    ListReverseInvolution(xs[1..]);
    ListReverseAppend(ListReverse(xs[1..]), [xs[0]]);
  }
}

function ListHead<T>(xs: seq<T>): Option<T> {
  if |xs| == 0 then None
  else Some(xs[0])
}

function ListTail<T>(xs: seq<T>): seq<T> {
  if |xs| == 0 then []
  else xs[1..]
}

function ListTake<T>(xs: seq<T>, n: int): seq<T> {
  if n <= 0 then []
  else if n >= |xs| then xs
  else xs[..n]
}

function ListDrop<T>(xs: seq<T>, n: int): seq<T> {
  if n <= 0 then xs
  else if n >= |xs| then []
  else xs[n..]
}

function ListZip<A, B>(xs: seq<A>, ys: seq<B>): seq<(A, B)>
  decreases |xs|
{
  if |xs| == 0 || |ys| == 0 then []
  else [(xs[0], ys[0])] + ListZip(xs[1..], ys[1..])
}

function ListFind<T>(xs: seq<T>, p: T -> bool): Option<T>
  decreases |xs|
{
  if |xs| == 0 then None
  else if p(xs[0]) then Some(xs[0])
  else ListFind(xs[1..], p)
}

function ListAny<T>(xs: seq<T>, p: T -> bool): bool
  decreases |xs|
{
  if |xs| == 0 then false
  else p(xs[0]) || ListAny(xs[1..], p)
}
