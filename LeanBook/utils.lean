module
import LeanBook.doc

-- ---------------------------------------------------------------------------
-- Utility helpers
-- ---------------------------------------------------------------------------

/-- HTML-escape a plain-text string (for text nodes). -/
public def escapeHtml (s : String) : String :=
  s.replace "&" "&amp;" |>.replace "<" "&lt;" |>.replace ">" "&gt;"

/-- Look up a label in the cross-reference registry.
    Returns `(url, displayText)` if found, or `none` otherwise. -/
public def lookupLabel (label : String)
    (labels : List (String × (String × String))) : Option (String × String) :=
  (labels.find? (fun (k, _) => k == label)).map (fun (_, v) => v)

/-- Map an EnvType to its CSS class suffix. -/
public def envClass : EnvType → String
  | .thm        => "thm"
  | .lem        => "lem"
  | .prop       => "prop"
  | .cor        => "cor"
  | .defn       => "defn"
  | .exampleEnv => "example"
  | .remark     => "remark"
  | .proof      => "proof"

/-- Map an EnvType to its display name. -/
public def envName : EnvType → String
  | .thm        => "Theorem"
  | .lem        => "Lemma"
  | .prop       => "Proposition"
  | .cor        => "Corollary"
  | .defn       => "Definition"
  | .exampleEnv => "Example"
  | .remark     => "Remark"
  | .proof      => "Proof"

/-- True for environments that carry a sequential number. -/
public def envIsNumbered : EnvType → Bool
  | .proof  => false
  | .remark => false
  | _       => true

/-- Extract plain text from a list of inlines (for HTML title attributes). -/
public def inlinesToPlainText : List Inline → String
  | []                      => ""
  | (.text s)      :: rest  => s ++ inlinesToPlainText rest
  | (.emph cs)     :: rest  => inlinesToPlainText cs ++ inlinesToPlainText rest
  | (.bold cs)     :: rest  => inlinesToPlainText cs ++ inlinesToPlainText rest
  | (.code s)      :: rest  => s ++ inlinesToPlainText rest
  | (.math _)      :: rest  => inlinesToPlainText rest
  | (.link cs _)   :: rest  => inlinesToPlainText cs ++ inlinesToPlainText rest
  | (.ref l)       :: rest  => s!"[{l}]" ++ inlinesToPlainText rest
  | (.footnote cs) :: rest  => inlinesToPlainText cs ++ inlinesToPlainText rest

/-- Prepend an `id` attribute when a label is present.
    Reduces boilerplate in renderBlock for headings, math, figures, and envs. -/
public def addLabelAttr (label : Option String)
    (attrs : List (String × String)) : List (String × String) :=
  match label with
  | some l => ("id", l) :: attrs
  | none   => attrs
