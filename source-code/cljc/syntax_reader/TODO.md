
- A 'grey-zones/remove-commented-parts' fgv. ':fix-indents?' beállítása minden iterációban
  az aktuálisan kivágott rész utáni következő nem üres (szóköz, sortörés, ...) karakter
  soron belüli pozícióját állítja helyre:

Eredeti szöveg:

```
(defn my-function
  []
  (letfn [; My comment
          (f0 [] ...)]))
```

Komment nyitó tag: `;`

Komment záró tag: `\n`

Komment kivágása utáni szöveg:

```
(defn my-function
  []
  (letfn [          (f0 [] ...)]))
```

A függvénynek két lehetősége van kijavítani az indent-et:
A) A kivágás helye és a következő nem üres karakter közötti szóközök számának csökkentésével
B) A kivágás helye és a következő nem üres karakter közötti szóközök számának növelésével esetleg sörtörés beiktatásával

A)

```
(defn my-function
  []
  (letfn [(f0 [] ...)]))
```

B)

```
(defn my-function
  []
  (letfn [
          (f0 [] ...)]))
```

Nyilván az A-t próbálja először

- A 'grey-zones/remove-commented-parts' fgv. ':remove-leftover-newlines?' beállítása minden
  iterációban az aktuálisan kivágott rész helyét megvizsgálja és ha az egy üres sorba esik akkor törli a sort:

```
.my-class {
  /* My comment */
  width: auto;
}
```

=>

```
.my-class {

  width: auto;
}
```

Ezt az üres sort kell törölni a komment kivágása után
