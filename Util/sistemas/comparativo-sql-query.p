DEFINE VARIABLE txt-codfam AS CHARACTER FORMAT "X(3)":U 
     LABEL "Línea" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE txt-Desde AS CHARACTER FORMAT "X(6)":U 
     LABEL "Articulo" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE txt-fchcorte AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha Corte" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE txt-Hasta AS CHARACTER FORMAT "X(6)":U 
     LABEL "Articulo" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE txt-Proveedor AS CHARACTER FORMAT "X(256)":U 
     LABEL "Algun Proveedor" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE txt-subfam AS CHARACTER FORMAT "X(3)":U 
     LABEL "Sub-Línea" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE rs-estado AS CHARACTER INITIAL "T" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Activo", "A",
"Baja Rotación", "B",
"Desactivo", "D",
"Todos", "T"
     SIZE 47 BY 1.08 NO-UNDO.

DEFINE VARIABLE rs-tipo AS CHARACTER INITIAL "D" 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Detallado", "D",
"Resumen", "R"
     SIZE 21 BY 1.69 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 91 BY 13.65.

DEFINE VARIABLE tg-stock AS LOGICAL INITIAL yes 
     LABEL "Solo Con Stock" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY .77 NO-UNDO.
    
DEF TEMP-TABLE t-matg NO-UNDO LIKE almmmatg.

DEF VAR A AS INT64 NO-UNDO.
    
a = ETIME(YES).

/*     FOR EACH Almmmatg WHERE Almmmatg.codcia = 001 AND ~                                */
/*             (TRUE <> (txt-Desde > '') OR Almmmatg.codmat >= txt-Desde) AND ~           */
/*             (TRUE <> (txt-Hasta > '') OR Almmmatg.codmat <= txt-Hasta) AND ~           */
/*             (TRUE <> (txt-codfam > '') OR almmmatg.codfam = txt-codfam) AND ~          */
/*             (TRUE <> (txt-subfam > '') OR almmmatg.subfam = txt-subfam) AND ~          */
/*             /*(rs-estado = 'T' OR Almmmatg.tpoart = rs-estado) AND ~*/                 */
/*             (TRUE <> (txt-proveedor > '') OR almmmatg.CodPr1 = txt-proveedor) NO-LOCK: */
/*         CREATE t-Matg.                                                                 */
/*         BUFFER-COPY Almmmatg USING codcia codmat TO t-Matg.                            */
/*     END.                                                                               */

/*     DEF QUERY q-Matg FOR Almmmatg.                                                 */
/*     OPEN QUERY q-Matg FOR EACH Almmmatg WHERE Almmmatg.codcia = 1 AND ~            */
/*         (TRUE <> (txt-Desde > '') OR Almmmatg.codmat >= txt-Desde) AND ~           */
/*         (TRUE <> (txt-Hasta > '') OR Almmmatg.codmat <= txt-Hasta) AND ~           */
/*         (TRUE <> (txt-codfam > '') OR almmmatg.codfam = txt-codfam) AND ~          */
/*         (TRUE <> (txt-subfam > '') OR almmmatg.subfam = txt-subfam) AND ~          */
/*         /*(rs-estado = 'T' OR Almmmatg.tpoart = rs-estado) AND ~*/                 */
/*         (TRUE <> (txt-proveedor > '') OR almmmatg.CodPr1 = txt-proveedor) NO-LOCK. */
/*     GET FIRST q-Matg.                                                              */
/*     DO WHILE NOT QUERY-OFF-END('q-Matg'):                                          */
/*         CREATE t-Matg.                                                             */
/*         BUFFER-COPY Almmmatg USING codcia codmat TO t-Matg.                        */
/*         GET NEXT q-Matg.                                                           */
/*     END.                                                                           */
/*     CLOSE QUERY q-Matg.                                                            */

DEF VAR x-CodCia AS INTE.
DEF VAR x-CodMat AS CHAR.

DECLARE c-Matg CURSOR FOR 
SELECT codcia, codmat
    FROM Almmmatg 
    WHERE Almmmatg.codcia = 001 AND
        (txt-Desde = '' OR Almmmatg.codmat >= txt-Desde) AND ~
        (txt-Hasta = '' OR Almmmatg.codmat <= txt-Hasta) AND ~
        (txt-codfam = '' OR almmmatg.codfam = txt-codfam) AND ~
        (txt-subfam = '' OR almmmatg.subfam = txt-subfam) AND ~
        (txt-proveedor = '' OR almmmatg.CodPr1 = txt-proveedor) ~
    .
OPEN c-Matg.
FETCH c-Matg INTO x-CodCia, x-CodMat.
REPEAT:
    CREATE t-Matg.
    t-matg.codcia = x-codcia.
    t-matg.codmat = x-codmat.
    FETCH c-Matg INTO x-CodCia, x-CodMat.
END.
CLOSE c-Matg.


DISPLAY ETIME.
