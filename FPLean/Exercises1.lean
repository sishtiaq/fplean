-- Exercises 1.1.2
#eval 42+19
#eval String.append "A" (String.append "B" "C")
#eval String.append (String.append "A" "B") "C"
#eval if 3==3 then 5 else 7
#eval if 3==4 then "equal" else "not equal"

-- Exercise 1.3.1.1
def joinStingsWith (inner pre post : String) :=
  String.append pre (String.append inner post)

#check joinStingsWith ","

def vol (h w d : Nat) := h * w * d

-- Exercise 1.4.3
structure rectangularPrism where
  h : Float
  w : Float
  d : Float
deriving Repr

def volume (x : rectangularPrism) := x.h * x.w * x.d

structure Segment where
  start : Float × Float
  stop : Float × Float
deriving Repr

def length (s : Segment) : Float :=
  let (x1, y1) := s.start
  let (x2, y2) := s.stop
  Float.sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)

#check rectangularPrism.mk
#check rectangularPrism.h
#check rectangularPrism.w
#check rectangularPrism.d

structure Book where
  makeBook ::
  title : String
  author : String
  price : Float
deriving Repr

#check Book.makeBook
#check Book.title

-- Exercise 1.6.5

def lastInList (α : Type) (l : List α) : Option α :=
  match l with
  | [] => none
  | [x] => some x
  | _ :: xs => lastInList α xs

def List.findFirst? {α : Type} (xs : List α) (predicate : α → Bool) : Option α :=
  match xs with
  | [] => none
  | x :: xs' => if predicate x then some x else List.findFirst? xs' predicate

def Prod.switch {α β : Type} (pair : α × β) : β × α :=
  (pair.snd, pair.fst)

inductive petName where
  | dog (name : String)
  | cat (name : String)

def zip {α β : Type} (xs : List α) (ys : List β) : List (α × β) :=
  match xs, ys with
  | [], _ => []
  | _, [] => []
  | x :: xs', y :: ys' => (x, y) :: zip xs' ys'

def take_aux {α : Type} (n : Nat) (acc : List α) (xs : List α) : List α :=
  match n, xs with
  | 0, _ => acc
  | _n+1, [] => acc
  | n+1, x :: xs' => take_aux n (acc ++ [x]) xs'

def take (α : Type) (n : Nat) (xs : List α) : List α :=
  take_aux n [] xs

#eval take Nat 5 [1,2,3,4,5]

def prodOverSum (α β γ : Type) (x : α × (β ⊕ γ)) : (α × β) ⊕ (α × γ) :=
  match x with
  | (a, Sum.inl b) => Sum.inl (a, b)
  | (a, Sum.inr c) => Sum.inr (a, c)

def multByTwoSum (α : Type) (x : Bool × α) : α ⊕ α :=
  match x with
  | (true, a) => Sum.inl a
  | (false, a) => Sum.inr a
