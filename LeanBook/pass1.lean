module
import LeanBook.doc
import LeanBook.html
import LeanBook.utils

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

/-- Traverse blocks in Pass 1, mimicking the same counter increments as
    `renderBlock` so that numbering is identical in both passes.
    Labeled items are stored in `RenderState.labels`.

    This function is `partial` because `Block` values can be arbitrarily deep:
    `.env` contains a `List Block`, and `.list` / `.blockquote` / `.marginNote`
    also embed nested blocks, so Lean's termination checker cannot prove
    well-foundedness without additional annotation. -/
public partial def collectBlockLabels (chNum : Nat) (fileUrl : String)
    : Block → StateM RenderState Unit
  | .heading level _title label => do
      if level == 1 then
        modify (fun st => { st with sectionNum := st.sectionNum + 1 })
      match label with
      | none   => pure ()
      | some l => do
          let st ← get
          let displayText := s!"Section {chNum}.{st.sectionNum}"
          modify (fun st => { st with
            labels := (l, (s!"{fileUrl}#{l}", displayText)) :: st.labels })

  | .displayMath _tex label => do
      modify (fun st => { st with equationNum := st.equationNum + 1 })
      match label with
      | none   => pure ()
      | some l => do
          let st ← get
          let displayText := s!"({chNum}.{st.equationNum})"
          modify (fun st => { st with
            labels := (l, (s!"{fileUrl}#{l}", displayText)) :: st.labels })

  | .env envType _title label blocks => do
      if envIsNumbered envType then
        modify (fun st => { st with theoremNum := st.theoremNum + 1 })
      match label with
      | none   => pure ()
      | some l => do
          let st ← get
          let displayText :=
            if envIsNumbered envType
              then s!"{envName envType} {chNum}.{st.theoremNum}"
              else envName envType
          modify (fun st => { st with
            labels := (l, (s!"{fileUrl}#{l}", displayText)) :: st.labels })
      blocks.forM (collectBlockLabels chNum fileUrl)

  | .figure _url _caption label => do
      modify (fun st => { st with figureNum := st.figureNum + 1 })
      match label with
      | none   => pure ()
      | some l => do
          let st ← get
          let displayText := s!"Figure {chNum}.{st.figureNum}"
          modify (fun st => { st with
            labels := (l, (s!"{fileUrl}#{l}", displayText)) :: st.labels })

  | .diagram _svg _caption label => do
      modify (fun st => { st with figureNum := st.figureNum + 1 })
      match label with
      | none   => pure ()
      | some l => do
          let st ← get
          let displayText := s!"Figure {chNum}.{st.figureNum}"
          modify (fun st => { st with
            labels := (l, (s!"{fileUrl}#{l}", displayText)) :: st.labels })

  | .list items _ordered =>
      items.forM (fun blks => blks.forM (collectBlockLabels chNum fileUrl))

  | .blockquote blks => blks.forM (collectBlockLabels chNum fileUrl)
  | .marginNote blks => blks.forM (collectBlockLabels chNum fileUrl)
  | _                => pure ()

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
