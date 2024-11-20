
DEFINE VARIABLE cFile AS CHARACTER NO-UNDO.

FOR EACH pl-pers:
    cFile = "O:\OpenEdge\on_in_co\aplic\pln\img\" + codper + ".bmp".
    IF SEARCH(cFile) <> ? THEN
        COPY-LOB FROM FILE cFile TO foto.
    DISPLAY codper.
END.
