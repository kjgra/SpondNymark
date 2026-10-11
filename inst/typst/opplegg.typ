// Treningsopplegg som PDF. Leser data.json, som R lager i plan_pdf()
// (R/fct_plan.R). Skissene er PNG-filer i samme mappe.
// Farger som i appen: svart og gul, med grønne baner i skissene.

#let data = json("data.json")

#let black = rgb("#121212")
#let yellow = rgb("#FFC629")
#let yellow-soft = rgb("#FFF3CC")
#let yellow-ink = rgb("#6B4E00")
#let grey = rgb("#5E5E5E")
#let line-col = rgb("#E3E1DB")
#let ground = rgb("#F8F7F4")
// Barlow Condensed ligger i familien Barlow med smalere bredde.
#let cond = (font: "Barlow", stretch: 75%)

#set document(title: data.tittel, author: "Nymark – Gruppeorganisering")
#set page(
  paper: "a4",
  margin: (top: 1.9cm, bottom: 1.7cm, x: 1.8cm),
  background: place(top + left, block(width: 100%, height: 7mm, fill: black,
    place(bottom + left, block(width: 100%, height: 1.2mm, fill: yellow)))),
  footer: context {
    set text(size: 8pt, fill: grey)
    data.bunntekst
    h(1fr)
    [Side #counter(page).display() av #counter(page).final().first()]
  },
)
#set text(font: "Barlow", size: 10pt, lang: "nb", fill: black)
#set par(leading: 0.55em, spacing: 0.8em)
#set list(indent: 0pt, body-indent: 5pt, spacing: 0.5em)

// **fet** i teksten blir fet skrift. Ingen annen tekst tolkes som kode.
#let rich(s) = {
  let parts = s.split("**")
  for (i, part) in parts.enumerate() {
    if calc.odd(i) { strong(part) } else { part }
  }
}

#let section(title) = block(above: 13pt, below: 6pt, sticky: true,
  box(fill: yellow, width: 4pt, height: 11pt, baseline: 1pt) + h(6pt)
  + text(..cond, size: 13.5pt, weight: 700)[#title])

#let sub(title) = block(above: 10pt, below: 4pt, sticky: true,
  text(..cond, size: 12pt, weight: 700)[#title])

#let bullets(items) = if items.len() > 0 { list(..items.map(rich)) }

#let questions(items) = if items.len() > 0 {
  list(marker: text(weight: 700, fill: yellow-ink)[?], ..items.map(rich))
}

// Side 1: øktplan --------------------------------------------------------------

#text(..cond, size: 27pt, weight: 800)[#data.tittel]
#if data.undertittel != "" { v(-6pt); text(size: 11.5pt, fill: grey)[#data.undertittel] }
#if data.info.len() > 0 { v(-2pt); text(size: 9.5pt, weight: 600)[#data.info.join("  ·  ")] }

#if data.fokus.forsvar != "" or data.fokus.angrep != "" {
  block(fill: yellow-soft, inset: 10pt, radius: 4pt, width: 100%, above: 12pt)[
    #text(..cond, size: 13pt, weight: 700)[Dette jobber vi med]
    #grid(columns: (1fr, 1fr), gutter: 14pt,
      if data.fokus.forsvar != "" [*Forsvar:* #rich(data.fokus.forsvar)],
      if data.fokus.angrep != "" [*Angrep:* #rich(data.fokus.angrep)])
  ]
}

#if data.stikkord.len() > 0 {
  section[#if data.stikkord.len() == 1 [Stikkord for hele økta] else [#data.stikkord.len() stikkord for hele økta]]
  // Én rad i et rutenett, så boksene blir like høye.
  grid(columns: (1fr,) * data.stikkord.len(), column-gutter: 6pt, inset: 8pt,
    stroke: (top: 3pt + yellow, left: 0.6pt + line-col, right: 0.6pt + line-col, bottom: 0.6pt + line-col),
    ..data.stikkord.enumerate().map(((i, s)) => [
      #text(weight: 700)[#(i + 1). #s.tittel] \
      #rich(s.tekst)
    ]))
  if data.stikkord_merknad != "" { text(size: 9pt, fill: grey)[#rich(data.stikkord_merknad)] }
}

#section[Tidsplan#if data.tidsplan_tabell.columns.len() > 2 [ og rotasjon] · #data.varighet min]
#{
  let cols = data.tidsplan_tabell.columns
  let n = cols.len() - 1
  table(
    columns: (auto,) + (1fr,) * n,
    stroke: 0.5pt + line-col,
    inset: (x: 7pt, y: 5.5pt),
    fill: (x, y) => if y == 0 { black },
    ..cols.map(c => text(fill: white, weight: 700)[#c]),
    ..data.tidsplan_tabell.rows.map(r => {
      if r.felles {
        (table.cell(fill: yellow-soft)[#r.tid], table.cell(colspan: n, fill: yellow-soft)[#rich(r.tekst)])
      } else {
        ([#r.tid],) + r.celler.map(c => [#c])
      }
    }).flatten()
  )
}
#if data.tidsplan.merknad != "" { text(size: 9pt, fill: grey)[#rich(data.tidsplan.merknad)] }

#if data.avslutning_sporsmal.len() > 0 {
  section[Felles avslutning – spør, ikke fortell]
  questions(data.avslutning_sporsmal)
}

#if data.grupper.len() > 0 {
  section[Grupper]
  grid(columns: (1fr,) * calc.min(data.grupper.len(), 4), gutter: 6pt,
    ..data.grupper.map(g => block(width: 100%, inset: 8pt, fill: ground, radius: 4pt)[
      #text(weight: 700)[#g.navn] \
      #text(size: 9pt)[#g.spillere.join(", ")]
    ]))
}

#section[Tegnforklaring]
#image("tegnforklaring.png", width: 100%)

#if data.kilde != "" { v(4pt); text(size: 8.5pt, fill: grey)[#rich(data.kilde)] }

// Én side per øvelse ---------------------------------------------------------------

#for (i, ex) in data.ovelser.enumerate() {
  pagebreak()
  block(fill: black, inset: (x: 8pt, y: 5pt), radius: 3pt, below: 8pt,
    text(fill: yellow, weight: 700, size: 9pt, tracking: 0.6pt)[
      ØVELSE #(i + 1)#if ex.kategori != "" [ · #upper(ex.kategori)]
    ])
  text(..cond, size: 23pt, weight: 800)[#ex.navn]
  if ex.fokus != "" { v(-4pt); par[#text(weight: 700, fill: grey)[Fokus:] #text(fill: grey)[#rich(ex.fokus)]] }
  if "bilde" in ex {
    v(2pt)
    align(center, if ex.bilde_hoy { image(ex.bilde, height: 12.5cm) } else { image(ex.bilde, width: 100%) })
  }
  v(4pt)
  grid(columns: (1fr, 1fr), gutter: 18pt,
    [
      #if ex.organisering.len() > 0 { sub[Organisering]; bullets(ex.organisering) }
      #if ex.gjennomforing.len() > 0 { sub[Gjennomføring]; bullets(ex.gjennomforing) }
      #if ex.tilpasning.len() > 0 { sub[Tilpasning]; bullets(ex.tilpasning) }
    ],
    [
      #if ex.laeringsmomenter.len() > 0 { sub[Læringsmomenter]; bullets(ex.laeringsmomenter) }
      #if ex.sporsmal.len() > 0 { sub[Spørsmål til spillerne]; questions(ex.sporsmal) }
      #if ex.enklere != "" or ex.vanskeligere != "" {
        sub[Variasjoner]
        if ex.enklere != "" { par[*Enklere:* #rich(ex.enklere)] }
        if ex.vanskeligere != "" { par[*Vanskeligere:* #rich(ex.vanskeligere)] }
      }
      #if ex.nff_url != "" {
        sub[Hos NFF]
        text(size: 9pt)[#link(ex.nff_url)[#ex.nff_url]]
      }
    ])
}
