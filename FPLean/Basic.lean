def hello := "hello"

-- def main : IO Unit := do
--   let englishGreeting := IO.println "Hello!"
--   IO.println "Bonjour!"
--   englishGreeting



example {p q : Prop} (H_p : p) (H_q : q) : (p ∧ q) := by
  exact And.intro H_p H_q


#print False



def anything : p → ¬ p → q
  | hp, hnp => False.elim (hnp hp)
  -- intros hp hnp
  -- exact False.elim (hnp hp)

-- theorem tq1 {f g h : Prop} (hyp : ¬ g ∧ (¬ f → (g ∧ h))) : f := by
--   cases hyp with
--   | intro ng fgh =>
--     have pf : f := by
--       have contra : ¬ f → f := by
--         intro nf
--         have q_and_r : g ∧ h := fgh nf
--         cases q_and_r with
--         | intro g' h' =>
--           have h_false : False := ng g'
--           exact show f from False.elim h_false
--     exact show f from pf


-- T ND Q3.
theorem tq3 : (f g h : Prop) → ((f ∧ h) ∨ ¬ g) ∧ (¬ f → (h ∧ g)) → f := by
  intros f g h hyp
  cases hyp with
  | intro hyp_left hyp_right =>
    cases hyp_left with
    | inl fh =>
      cases fh with
      | intro fh_f fh_g => exact fh_f
    | inr ng =>
      have gf : f := by
        apply Classical.byContradiction
        intro (nf : ¬ f)
        have hg : h ∧ g := hyp_right nf
        cases hg with
        | intro hgh hgg =>
          have h_false : False := ng hgg
          exact False.elim h_false
      exact gf
