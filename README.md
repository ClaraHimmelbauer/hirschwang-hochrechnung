Liebe WIPOLIs und alle anderen,

dieses Repo enthält alle Files, die wir brauchen, um eine Hochrechnung für die Nationalratswahl 2024 zu rechnen.
Außerdem alle Codes, die ich zur Vorbereitung der Daten und für weitere Erklärungen verwendet habe.

### Voraussetzungen

Für die Teilnahme am Workshop solltet ihr in der Schule schon einmal gehört haben, was eine lineare Funktion ist (y = kx + d).
Ihr müsst einen Laptop dabei haben, auf dem Excel installiert ist - wir werden die Hochrechnung gemeinsam in Excel rechnen.

Außerdem solltet ihr folgende 2 Sachen in Excel vorbereiten:
- Vorbereitete Daten herunterladen. Die findet ihr hier: `data/master.xlsx`.
- "Datenanlyse Add-in" in Excel aktivieren. Eine Anleitung wie das geht gibts in `man/excel-datenanalyse-aktivieren.docx`.

### Plan

Um eine Hochrechnung zu rechnen schauen wir uns an
- Lineare Funktionen
- Multivariate lineare Funktionen (aka was ist eine Regression)
- "Vorhersagen" mittels Regressionen
- Umsetzen in Excel

Und wenn wir danach noch Zeit haben zeige ich euch
- D'Hondt: wie die Stimmen auf Mandate umgerechnet werden
- Wie man die Schwankungsbreite mittels Bootstrapping rechnet
- Wie man Hochrechnungen durch Clusteirng exakter macht
- Berücksichtigung von Wahlkarten
- Ablauf am Wahlabend

### Dokumentation

Die Dokumentation findet ihr hier: `man/Dokumentation.Rmd` 

### Kontakt

Wenn du eine Frage hast schreib mir auf: clara.himmelbauer@wu.ac.at

### Ordnerstruktur

- analysis: alles wo gerechnet wird
- data: Roh- und Enddaten
- man: "man" steht für "manuals". Hier sind alle Anleitungen etc.
- plot: Output-Plots, falls wir welche haben
- R: R-Fuktionen (eher für mich zum Herzeigen)
- report: alle Präsentationen etc.
