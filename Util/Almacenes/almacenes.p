DEFINE VARIABLE qbf-count    AS INTEGER NO-UNDO.
DEFINE VARIABLE qbf-governor AS INTEGER NO-UNDO.
 
DEFINE VARIABLE qbf-govcnt AS INTEGER NO-UNDO.
DEFINE VARIABLE qbf-loop   AS INTEGER NO-UNDO.
DEFINE VARIABLE qbf-time   AS INTEGER NO-UNDO.

DEFINE BUFFER Almacen FOR INTEGRAL.Almacen.

ASSIGN
  qbf-count    = 0
  qbf-governor = 0
  qbf-time     = TIME.

OUTPUT TO TERMINAL PAGED.

DO FOR INTEGRAL.Almacen:
  FORM HEADER
    "                              " +  "ALMACENES" FORMAT "x(68)" SKIP
    "                                                       " +  "Pagina " + TRIM(STRING(PAGE-NUMBER,">>>>9")) FORMAT "x(68)" SKIP(1) 
    WITH FRAME qbf-header PAGE-TOP COLUMN 1 WIDTH 88 NO-ATTR-SPACE 
    NO-VALIDATE NO-LABELS NO-BOX USE-TEXT STREAM-IO.
  VIEW FRAME qbf-header.

  main-loop:
  FOR EACH INTEGRAL.Almacen NO-LOCK
    WHERE (INTEGRAL.Almacen.CodCia = 1)
    BREAK BY INTEGRAL.Almacen.CodAlm:

    qbf-count  = qbf-count + 1.

    FORM
      INTEGRAL.Almacen.CodAlm COLUMN-LABEL "Almacén" FORMAT "x(3)"
      INTEGRAL.Almacen.Descripcion COLUMN-LABEL "Descripción" FORMAT "X(60)"
      WITH FRAME qbf-report-1 DOWN COLUMN 1 WIDTH 88
      NO-ATTR-SPACE NO-VALIDATE NO-BOX USE-TEXT STREAM-IO.

    DISPLAY
      INTEGRAL.Almacen.CodAlm
      INTEGRAL.Almacen.Descripcion
      WITH FRAME qbf-report-1.

    DOWN WITH FRAME qbf-report-1.
  END.

  PAGE.
END.

OUTPUT CLOSE.
RETURN.
