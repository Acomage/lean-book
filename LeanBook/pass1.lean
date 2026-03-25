module
public import LeanBook.doc
public import LeanBook.html
public import LeanBook.utils

-- ---------------------------------------------------------------------------
-- Pass 1: Collect all cross-reference labels
-- ---------------------------------------------------------------------------

/-- Reset the per-chapter counters in `RenderState` and record the new chapter
    number.  Called identically in both Pass 1 and `renderChapterPage` (Pass 2)
    to keep numbering in sync. -/
public def resetChapterCounters (chNum : Nat) (st : RenderState) : RenderState :=
  { st with
    chapterNum  := chNum
    sectionNum  := 0
    equationNum := 0
    figureNum   := 0
    theoremNum  := 0
  }

/-- Collect cross-reference labels from a single `AnyBlock`.
    This simply delegates to the block's own `collectLabels` method,
    which was stored as a closure when the block was wrapped via `AnyBlock.mk`. -/
public def collectBlockLabels (chNum : Nat) (fileUrl : String)
    (block : AnyBlock) : StateM RenderState Unit :=
  block.collectLabels chNum fileUrl

/-- Reset per-chapter counters, register the chapter label if present,
    then collect all block labels for chapter `chNum`. -/
public def collectChapterLabels (chNum : Nat) (ch : Chapter) : StateM RenderState Unit := do
  modify (resetChapterCounters chNum)
  let fileUrl := s!"chapter-{chNum}.html"
  match ch.label with
  | none   => pure ()
  | some l =>
      let displayText := s!"Chapter {chNum}"
      modify (fun st => { st with
        labels := (l, (s!"{fileUrl}#{l}", displayText)) :: st.labels })
  ch.blocks.forM (collectBlockLabels chNum fileUrl)

/-- Pass 1: walk the whole document and populate `RenderState.labels`.
    Call this before the rendering loop; use the resulting state as the
    initial state for Pass 2 so that all cross-references are resolved. -/
public def collectDocumentLabels (doc : Document) : StateM RenderState Unit :=
  doc.chapters.zipIdx.forM (fun (ch, i) => collectChapterLabels (i + 1) ch)
