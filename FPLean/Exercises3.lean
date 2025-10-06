-- Exercises 3.1.6.1

-- Replace
-- inductive Pos : Type where
--   | one : Pos
--   | succ : Pos → Pos
-- with
structure Pos where
  succ ::
  pred : Nat


#check Pos.succ (Nat.zero)

def Pos.Add : Pos -> Pos -> Pos
  | a, Pos.succ Nat.zero => Pos.succ a.pred
  | a, Pos.succ b => Pos.succ (a.pred + b + 1)

instance : Add Pos where
  add := Pos.Add

def Pos.mul : Pos -> Pos -> Pos
  | a, Pos.succ Nat.zero => a
  | a, Pos.succ b => Pos.succ ((a.pred + 1) * (b + 1) - 1)

instance : Mul Pos where
  mul := Pos.mul

instance : ToString Pos where
  toString p := toString (p.pred + 1)

instance : OfNat Pos n where
  ofNat := Pos.succ (n - 1)

def three : Pos := Pos.succ 2
def two : Pos := Pos.succ 1

#eval s!"3+2={three + two}, 3*2={three * two}"

-- Exercises 3.1.6.2
