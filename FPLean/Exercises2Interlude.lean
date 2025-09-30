-- 2->3 Interlude Exercises

def l : List String := ["Hello", ", ", "world", "!"]
#eval l[0]

theorem andormy {a b} : a ∧ b → a ∨ b := by
  intro h
  cases h
  . right; assumption

theorem sums : 2+3=5 ∧ 15-8=7 ∧ "hello".append " world" = "hello world" := by
  apply And.intro
  . rfl
  apply And.intro
  . rfl
  . rfl

theorem fivelesseight : 5 < 8 := by
  -- rlf
  simp

theorem sums_again : 2+3=5 ∧ 15-8=7 ∧ "hello".append " world" = "hello world" := by
  decide

def fifth (nn:List Nat) (ok: nn.length > 5): Nat := nn[5]
#eval fifth [0,1,2,3,4,5,6,7,8,9] (by decide)
