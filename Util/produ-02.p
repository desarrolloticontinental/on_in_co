def var x-base as dec.
def var x-hormen as dec.
def var x-factor as dec.
def var x-imphor as dec.
def var x-horai as dec.
def var x-segf as dec.
def var x-segi as dec.
DEFINE VAR X-HORA AS DECI EXTENT 10 FORMAT ">>>9.99".
  DEFINE VAR X-TOTA AS DECI EXTENT 10 FORMAT ">>>>>9.99".

def temp-table detalle
    field fecha as date
    field numord as char
    field codper as char
    field nomper as char
    field horas as dec
    field unitario as dec.
    

FOR EACH PR-MOV-MES NO-LOCK WHERE PR-MOV-MES.CodCia = 001 
        AND  PR-MOV-MES.FchReg >= 01/01/2006
        AND  PR-MOV-MES.FchReg <= 11/01/2006,
        FIRST PL-PERS WHERE PL-PERS.codper = PR-MOV-MES.codper NO-LOCK
        BREAK BY PR-MOV-MES.NumOrd BY PR-MOV-MES.FchReg BY PR-MOV-MES.CodPer :
    X-HORAI  = PR-MOV-MES.HoraI.
    X-HORA   = 0.
    X-IMPHOR = 0.
    X-TOTA   = 0.
    X-BASE   = 0.
    X-HORMEN = 0.
    X-FACTOR = 0.
    /* INGRESO POR PLANILLA */
    FOR EACH PL-MOV-MES NO-LOCK WHERE PL-MOV-MES.Codcia = PR-MOV-MES.Codcia 
        AND PL-MOV-MES.Periodo = PR-MOV-MES.Periodo 
        AND PL-MOV-MES.Nromes  = PR-MOV-MES.NroMes 
        AND PL-MOV-MES.CodPln  = 01 
        AND PL-MOV-MES.Codcal  = 0 
        AND PL-MOV-MES.CodPer = PR-MOV-MES.CodPer 
        AND (PL-MOV-MES.CodMov = 101 OR PL-MOV-MES.CodMov = 103):
      X-BASE = X-BASE + PL-MOV-MES.VALCAL-MES .                     
    END.
    /* VALOR POR MINUTO */
    FIND LAST PL-VAR-MES WHERE
        PL-VAR-MES.Periodo = PR-MOV-MES.Periodo AND
        PL-VAR-MES.NroMes  = PR-MOV-MES.NroMes 
        NO-ERROR.
    IF AVAILABLE PL-VAR-MES 
    THEN ASSIGN
            X-HORMEN = PL-VAR-MES.ValVar-MES[11]
            X-FACTOR = PL-VAR-MES.ValVar-MES[12] + PL-VAR-MES.ValVar-MES[13] + PL-VAR-MES.ValVar-MES[3] + PL-VAR-MES.ValVar-MES[5] + PL-VAR-MES.ValVar-MES[10].
            X-IMPHOR = (X-BASE / X-HORMEN) * ( 1 + (X-FACTOR / 100) ) / 60.
    FOR EACH PR-CFGPL NO-LOCK WHERE PR-CFGPL.Codcia = PR-MOV-MES.CodCia:
        IF X-HORAI >= PR-CFGPL.HoraI AND X-HORAI < PR-CFGPL.HoraF THEN DO:
            IF PR-MOV-MES.HoraF <= PR-CFGPL.HoraF THEN DO:
                X-SEGF = TRUNCATE(PR-MOV-MES.HoraF,0) * 60 + (PR-MOV-MES.HoraF - TRUNCATE(PR-MOV-MES.HoraF,0)) * 100 . 
                X-SEGI = TRUNCATE(X-HORAI,0) * 60 + (X-HORAI - TRUNCATE(X-HORAI,0)) * 100 . 
                X-HORA[PR-CFGPL.Nroitm] = X-HORA[PR-CFGPL.Nroitm] + (X-SEGF - X-SEGI) .                 
                X-TOTA[PR-CFGPL.Nroitm] = X-TOTA[PR-CFGPL.Nroitm] + (X-SEGF - X-SEGI) * PR-CFGPL.Factor * X-IMPHOR .                 
                X-TOTA[10] = X-TOTA[10] + (X-SEGF - X-SEGI) * PR-CFGPL.Factor * X-IMPHOR .                 
                LEAVE.
            END.
            IF PR-MOV-MES.HoraF > PR-CFGPL.HoraF THEN DO:
                X-SEGF = TRUNCATE(PR-CFGPL.HoraF,0) * 60 + (PR-CFGPL.HoraF - TRUNCATE(PR-CFGPL.HoraF,0)) * 100 . 
                X-SEGI = TRUNCATE(X-HORAI,0) * 60 + (X-HORAI - TRUNCATE(X-HORAI,0)) * 100 . 
                X-HORA[PR-CFGPL.Nroitm] = X-HORA[PR-CFGPL.Nroitm] + (X-SEGF - X-SEGI) .                 
                X-TOTA[PR-CFGPL.Nroitm] = X-TOTA[PR-CFGPL.Nroitm] + (X-SEGF - X-SEGI) * PR-CFGPL.Factor * X-IMPHOR .                 
                X-TOTA[10] = X-TOTA[10] + (X-SEGF - X-SEGI) * PR-CFGPL.Factor * X-IMPHOR .                 
                X-HORAI = PR-CFGPL.HoraF.
            END.
        END.
    END.
    X-IMPHOR = X-IMPHOR * 60.         
    FIND DETALLE WHERE DETALLE.fecha = PR-MOV-MES.FchReg
        AND DETALLE.numord = PR-MOV-MES.NumOrd
        AND DETALLE.codper = PR-MOV-MES.CodPer
        EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE DETALLE THEN DO:
        CREATE DETALLE.
        ASSIGN
            DETALLE.fecha = PR-MOV-MES.FchReg
            DETALLE.numord = PR-MOV-MES.NumOrd
            DETALLE.codper = PR-MOV-MES.CodPer.
    END.
    ASSIGN
        DETALLE.horas = DETALLE.horas + x-Hora[1]
        DETALLE.unitario = x-ImpHor
        DETALLE.nomper = TRIM(PL-PERS.patper) + ' ' + TRIM(PL-PERS.matper) + ' ' + PL-PERS.nomper.
END.

output to c:\tmp\mano-obra-val-oct.txt.
FOR EACH DETALLE:
    DETALLE.horas = ROUND(DETALLE.horas / 60, 2).
    DISPLAY
        detalle.fecha
        detalle.numord
        detalle.codper
        detalle.nomper format 'x(40)'
        detalle.horas
        detalle.unitario
        with stream-io no-box width 200.
END.
output close.

