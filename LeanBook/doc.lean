/-!
# Book AST (Abstract Syntax Tree)

This module defines the core data structures for representing a technical/mathematical book.
It is designed to support rich text, math environments (theorems, proofs), figures,
and side-column content (margin notes, remarks).
-/

/-- Types of mathematical and technical environments. -/
inductive EnvType where
  | thm | lem | prop | cor
  | defn | exampleEnv | remark | proof
  deriving Repr, BEq

/-- Inline elements flow within a paragraph or heading. -/
inductive Inline where
  | text (s : String)
  | emph (content : List Inline)
  | bold (content : List Inline)
  | code (s : String)
  | math (tex : String)
  | link (content : List Inline) (url : String)
  | ref (label : String)
  | footnote (content : List Inline)
  deriving Repr, Inhabited

/-- Block elements form the vertical flow of the document. -/
inductive Block where
  | para (inlines : List Inline)
  | heading (level : Nat) (title : List Inline) (label : Option String)
  | displayMath (tex : String) (label : Option String)
  | codeBlock (lang : String) (source : String)
  | list (items : List (List Block)) (ordered : Bool)
  | blockquote (blocks : List Block)
  | figure (url : String) (caption : Option (List Inline)) (label : Option String)
  | diagram (svg : String) (caption : Option (List Inline)) (label : Option String)
  /-- Mathematical environments like Theorem, Proof, Definition -/
  | env (type : EnvType) (title : Option (List Inline)) (label : Option String) (blocks : List Block)
  /-- Explicit margin notes to be placed in the right blank column -/
  | marginNote (blocks : List Block)
  deriving Repr, Inhabited

/-- A chapter is the top-level division of the book.
    Since we generate a separate HTML page per chapter,
    it explicitly carries its own blocks. -/
structure Chapter where
  title  : List Inline
  label  : Option String := none
  blocks : List Block
  deriving Repr, Inhabited

/-- The entire document/book. -/
structure Document where
  title    : List Inline
  author   : String := ""
  chapters : List Chapter
  deriving Repr, Inhabited