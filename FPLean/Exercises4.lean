
#check IO.println

def List.sumOfContents [Add α] [Zero α] : List α → α
  | [] => 0
  | x :: xs => x + xs.sumOfContents

#check Zero

theorem fun_eq : (fun x => x + 1) = (Nat.succ) := by
  rfl

#eval Functor.map (· + 5) [1, 2, 3]
