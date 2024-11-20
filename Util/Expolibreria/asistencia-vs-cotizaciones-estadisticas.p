DEF VAR x-imptot AS DEC NO-UNDO.
DEF VAR x-impest AS DEC NO-UNDO.
DEF VAR x-linea AS CHAR FORMAT 'x(100)'.

DEF TEMP-TABLE detalle
    FIELD codcli AS CHAR FORMAT 'x(11)'
    FIELD nomcli AS CHAR FORMAT 'x(60)'
    FIELD impcot AS DEC
    FIELD impest AS DEC
    FIELD fecha LIKE expasist.fecha
    FIELD fchped LIKE faccpedi.fchped
    FIELD estado LIKE expasist.estado
    FIELD situacion AS CHAR FORMAT 'x(15)' EXTENT 2
    FIELD codacti LIKE expasist.codacti
    FIELD hora LIKE expasist.hora
    INDEX llave01 codcli.

INPUT FROM c:\tmp\clientes.prn.
REPEAT:
    IMPORT UNFORMATTED x-linea.
    IF SUBSTRING(x-linea,1,11) <> "" THEN DO:
        CREATE detalle.
        ASSIGN
            detalle.codcli = SUBSTRING(x-linea,1,11)
            detalle.nomcli = SUBSTRING(x-linea,15,66)
            detalle.impest = DECIMAL(SUBSTRING(x-linea,81,15))
            detalle.situacion[1] = "NO INVITADO".
    END.
END.
INPUT CLOSE.

FOR EACH expasist NO-LOCK WHERE codcia = 1
    AND coddiv = '00015'
    AND fecha >= 10/25/2010,
    FIRST gn-clie WHERE gn-clie.codcia = 0
    AND gn-clie.codcli = expasist.codcli:
    FIND FIRST detalle WHERE detalle.codcli = expasist.codcli EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE detalle THEN DO:
        CREATE detalle.
        ASSIGN
            detalle.codcli = gn-clie.codcli
            detalle.nomcli = gn-clie.nomcli.
    END.
    ASSIGN
        detalle.fecha  = expasist.fecha
        detalle.codacti = expasist.codacti
        detalle.situacion[1] = "INVITADO"
        detalle.situacion[2] = (IF expasist.estado[1] = "C" THEN "SI ASISTIO" ELSE "NO ASISTIO")
        detalle.estado[5] = expasist.estado[5]
        detalle.hora = expasist.hora.
END.
FOR EACH faccpedi NO-LOCK WHERE faccpedi.codcia = 1
        AND faccpedi.coddoc = "cot"
        AND faccpedi.coddiv = "00015"
        AND faccpedi.fchped >= 10/25/2010
        AND LOOKUP (faccpedi.flgest, "P,C") > 0:
    FIND FIRST detalle WHERE detalle.codcli = faccpedi.codcli
        EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE detalle THEN DO:
        CREATE detalle.
        ASSIGN
            detalle.codcli = faccpedi.codcli
            detalle.nomcli = faccpedi.nomcli
            detalle.situacion[1] = "NO INVITADO"
            detalle.situacion[2] = "SI ASISTIO".
    END.
    ASSIGN
        detalle.fchped = faccpedi.fchped
        detalle.impcot = detalle.impcot + faccpedi.imptot.
END.

OUTPUT TO c:\tmp\informe.txt.
FOR EACH detalle:
    DISPLAY
        detalle.codcli  COLUMN-LABEL "Cliente"
        detalle.nomcli  COLUMN-LABEL " |"
        "|"
        detalle.fecha   COLUMN-LABEL "Fecha Invitacion |"
        "|"
        detalle.codacti COLUMN-LABEL "Horario Invitacion |"
        "|"
        detalle.estado[5] FORMAT "x(8)" COLUMN-LABEL "Fecha de asistencia |"
        "|"
        detalle.hora    COLUMN-LABEL "Hora de asistencia |"
        "|"
        detalle.situacion[1] FORMAT 'x(15)' COLUMN-LABEL "Estado 1 |"
        "|"
        detalle.situacion[2] FORMAT 'x(15)' COLUMN-LABEL "Estado 2 |"
        "|"
        detalle.impest  FORMAT '(>>>,>>>,>>9.99)' COLUMN-LABEL "Historico |"
        "|"
        detalle.impcot  FORMAT '(>>>,>>>,>>9.99)' COLUMN-LABEL "Cotizado |"
        "|"
        detalle.fchped   COLUMN-LABEL "Fecha de cotizacion |"
        "|"
        WITH STREAM-IO NO-BOX NO-UNDERLINE WIDTH 320
        .
END.
OUTPUT CLOSE.

