module
public import LeanBook.html
/-!
# Book AST (Abstract Syntax Tree)

This module defines the core data structures for representing a technical/mathematical book.
It is designed to be highly extensible: new block-level types can be introduced by implementing
the `BlockNode` typeclass, without modifying this file.

## Architecture overview

* `Inline` – a *closed* inductive type for inline content (text, math, code, links, …).
  Inline elements are rarely extended by end-users, so a closed type is fine here.

* `BlockNode` – the *open* typeclass that every block element must satisfy.
  It exposes `toHtml`, `toSource`, and `collectLabels`.

* `AnyBlock` – a type-erased wrapper that holds any `BlockNode` value.
  Documents store `List AnyBlock`, enabling heterogeneous block sequences.

* Concrete block structs (`ParaBlock`, `HeadingBlock`, …) – the built-in implementations.
  Users can define additional structs and provide `BlockNode` instances for them.
-/

/-- Types of mathematical and technical environments. -/
public inductive EnvType where
  | thm | lem | prop | cor
  | defn | exampleEnv | remark | proof
  deriving Repr, BEq

/-- Inline elements flow within a paragraph or heading. -/
public inductive Inline where
  | text (s : String)
  | emph (content : List Inline)
  | bold (content : List Inline)
  | code (s : String)
  | math (tex : String)
  | link (content : List Inline) (url : String)
  | ref (label : String)
  | footnote (content : List Inline)
  deriving Repr, Inhabited

/-!
## Extensible block architecture
-/

/-- The core typeclass that every block-level document element must satisfy.

    Implement this for any custom type to make it usable inside a `Document`
    **without** modifying this file.

    * `toHtml`        – render to HTML inside the `RenderM` monad.
    * `toSource`      – recover the original source text (used for copy interception).
    * `collectLabels` – register cross-reference labels during Pass 1 (defaults to a no-op).
      Parameters: the block value, `chNum` (1-based chapter number), and `fileUrl`
      (URL of the chapter HTML file, used to build anchor links for labels).
-/
public class BlockNode (α : Type) where
  toHtml        : α → RenderM Html
  toSource      : α → String
  collectLabels : α → Nat → String → StateM RenderState Unit :=
    fun _ _ _ => pure ()

/-- A type-erased wrapper that holds any value implementing `BlockNode`.

    Use `AnyBlock.mk` to wrap a concrete block value.
    The three methods are stored as closures so that the wrapper is self-contained. -/
public structure AnyBlock where
  /-- Internal raw constructor; use `AnyBlock.mk` for type-safe construction. -/
  raw ::
  toHtmlFn        : RenderM Html
  toSourceFn      : String
  collectLabelsFn : Nat → String → StateM RenderState Unit

/-- Render this block to HTML. -/
public def AnyBlock.toHtml (b : AnyBlock) : RenderM Html := b.toHtmlFn

/-- Extract the original source text. -/
public def AnyBlock.toSource (b : AnyBlock) : String := b.toSourceFn

/-- Collect cross-reference labels (Pass 1). -/
public def AnyBlock.collectLabels (b : AnyBlock) (chNum : Nat) (fileUrl : String)
    : StateM RenderState Unit :=
  b.collectLabelsFn chNum fileUrl

/-- Wrap any `BlockNode` value into an `AnyBlock`.

    After wrapping, the concrete type is erased; only the three interface
    methods (`toHtml`, `toSource`, `collectLabels`) remain accessible. -/
public def AnyBlock.mk [BlockNode α] (a : α) : AnyBlock :=
  { toHtmlFn        := BlockNode.toHtml a,
    toSourceFn      := BlockNode.toSource a,
    collectLabelsFn := fun chNum fileUrl => BlockNode.collectLabels a chNum fileUrl }

/-!
## Concrete block types

Each struct below is a built-in implementation of `BlockNode`.
The actual `BlockNode` instances are provided in `render.lean`
(where `renderInline` is available), so only the data definitions live here.
-/

/-- A paragraph of inline content. -/
public structure ParaBlock where
  inlines : List Inline

/-- A section/subsection heading. -/
public structure HeadingBlock where
  level : Nat
  title : List Inline
  label : Option String

/-- A display (block) math equation. -/
public structure DisplayMathBlock where
  tex   : String
  label : Option String

/-- A fenced code block. -/
public structure CodeBlock where
  lang   : String
  source : String

/-- A bullet or numbered list whose items are themselves sequences of blocks. -/
public structure ListBlock where
  items   : List (List AnyBlock)
  ordered : Bool

/-- A blockquote. -/
public structure BlockquoteBlock where
  blocks : List AnyBlock

/-- A figure displaying an image. -/
public structure FigureBlock where
  url     : String
  caption : Option (List Inline)
  label   : Option String

/-- An inline SVG diagram. -/
public structure DiagramBlock where
  svg     : String
  caption : Option (List Inline)
  label   : Option String

/-- A mathematical environment such as Theorem, Definition, or Proof. -/
public structure EnvBlock where
  type   : EnvType
  title  : Option (List Inline)
  label  : Option String
  blocks : List AnyBlock

/-- A margin note displayed in the right-hand column. -/
public structure MarginNoteBlock where
  blocks : List AnyBlock

/-!
## Document structure
-/

/-- A chapter is the top-level division of the book.
    Since we generate a separate HTML page per chapter,
    it explicitly carries its own list of (heterogeneous) blocks. -/
public structure Chapter where
  title  : List Inline
  label  : Option String := none
  blocks : List AnyBlock

/-- The entire document/book. -/
public structure Document where
  title    : List Inline
  author   : String := ""
  chapters : List Chapter
