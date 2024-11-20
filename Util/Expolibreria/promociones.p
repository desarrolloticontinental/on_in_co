DEF VAR s-codcia AS INT INIT 001.

DEF TEMP-TABLE Detalle
    FIELD codmat LIKE FacDPedi.codmat
    FIELD canped LIKE FacDPedi.canped
    FIELD implin LIKE FacDPedi.implin
    FIELD impmin AS DEC         /* Importes y cantidades minimas */
    FIELD canmin AS DEC.

DEF TEMP-TABLE Promocion LIKE FacDPedi.
DEF TEMP-TABLE ITEM LIKE Facdpedi
    FIELD NroDoc LIKE VtaCProm.NroDoc
    FIELD Tipo   AS CHAR.

FOR EACH faccpedi NO-LOCK WHERE codcia = s-codcia
    AND coddoc = 'cot'
    AND coddiv = '00015'
    AND tpoped = 'e12'
    AND fchped >= 01/04/2012
    AND LOOKUP(flgest, 'w,a') = 0:
    RUN Promociones.
END.

OUTPUT TO c:\tmp\promociones.txt.
FOR EACH ITEM BY ITEM.nroped BY ITEM.nrodoc BY ITEM.tipo BY ITEM.codmat:
    DISPLAY
        ITEM.nroped
        ITEM.codmat
        ITEM.canped
        ITEM.implin
        ITEM.tipo   FORMAT 'x(10)'
        ITEM.nrodoc COLUMN-LABEL 'N° promocion'
        WITH STREAM-IO NO-BOX WIDTH 320.
END.
OUTPUT CLOSE.


PROCEDURE promociones:

IF LOOKUP (FacCPedi.FmaPgo, '900,999') > 0 THEN RETURN.

DEF VAR x-ImpLin AS DEC.
DEF VAR x-CanDes AS DEC.
DEF VAR x-ImpMin AS DEC.
DEF VAR x-CanMin AS DEC.
DEF VAR x-Factor AS INT.
DEFINE VARIABLE F-PREBAS LIKE Almmmatg.PreBas NO-UNDO.
DEFINE VARIABLE F-DSCTOS LIKE Almmmatg.PorMax NO-UNDO.
DEFINE VARIABLE F-PREVTA AS DECI NO-UNDO.
DEFINE VARIABLE Y-DSCTOS AS DECI NO-UNDO.
DEFINE VARIABLE SW-LOG1  AS LOGI NO-UNDO.
DEFINE VARIABLE X-TIPDTO AS CHAR NO-UNDO.
DEFINE VARIABLE I-NITEM AS INTEGER NO-UNDO INIT 0.

/* Barremos las promociones activas */
FOR EACH Vtacprom NO-LOCK WHERE Vtacprom.codcia = FacCPedi.codcia
    AND Vtacprom.coddiv = FacCPedi.coddiv
    AND Vtacprom.coddoc = 'PRO'
    AND Vtacprom.FlgEst = 'A'
    AND Vtacprom.codpro = '54456344'
    AND (TODAY >= VtaCProm.Desde AND TODAY <= VtaCProm.Hasta):
    /* Acumulamos los productos promocionables */
    EMPTY TEMP-TABLE Detalle.   /* Limpiamos temporal */
    EMPTY TEMP-TABLE Promocion.
    CASE Vtacprom.TipProm:
        WHEN 1 OR WHEN 2 THEN DO:
            FOR EACH Vtadprom OF Vtacprom NO-LOCK WHERE Vtadprom.Tipo = 'P':
                FIND FacDPedi OF FacCPedi WHERE FacDPedi.codmat = Vtadprom.codmat
                    NO-LOCK NO-ERROR.
                IF AVAILABLE FacDPedi THEN DO:
                    CREATE ITEM.
                    BUFFER-COPY Facdpedi TO ITEM
                        ASSIGN 
                            ITEM.NroDoc = VtaCProm.NroDoc
                            ITEM.Tipo   = "PRODUCTO".

                    FIND Detalle WHERE Detalle.codmat = FacDPedi.codmat NO-ERROR.
                    IF NOT AVAILABLE Detalle THEN CREATE Detalle.
                    ASSIGN
                        Detalle.codmat = Vtadprom.codmat
                        Detalle.canped = Detalle.canped + ( FacDPedi.canped * FacDPedi.Factor )
                        Detalle.implin = Detalle.implin + FacDPedi.implin
                        Detalle.impmin = Vtadprom.importe
                        Detalle.canmin = Vtadprom.cantidad.
                END.
            END.
        END.
        WHEN 3 THEN DO:
            FOR EACH Facdpedi OF Faccpedi NO-LOCK, FIRST Almmmatg OF Facdpedi NO-LOCK WHERE Almmmatg.codpr1 = Vtacprom.codpro:
                CREATE ITEM.
                BUFFER-COPY Facdpedi TO ITEM
                    ASSIGN 
                        ITEM.NroDoc = VtaCProm.NroDoc
                        ITEM.Tipo   = "PRODUCTO".
            END.
        END.
    END CASE.
    /* Generamos la promocion */
    PROMOCIONES:
    DO:
        x-Factor = 0.
        CASE Vtacprom.TipProm:
            WHEN 1 THEN DO:     /* Por Importes */
                x-ImpLin = 0.
                FOR EACH Detalle:
                    IF FacCPedi.CodMon = Vtacprom.codmon THEN x-ImpMin = Detalle.ImpMin.
                    ELSE IF FacCPedi.CodMon = 1 THEN x-ImpMin = Detalle.ImpMin * FacCPedi.TpoCmb.
                                                ELSE x-ImpMin = Detalle.ImpMin / FacCPedi.TpoCmb.
                    IF x-ImpMin > 0 AND x-ImpMin > Detalle.ImpLin THEN NEXT.
                    x-ImpLin = x-ImpLin + Detalle.ImpLin.
                END.
                x-ImpMin = Vtacprom.Importe.
                IF FacCPedi.CodMon <> Vtacprom.CodMon
                    THEN IF FacCPedi.CodMon = 1 THEN x-ImpLin = x-ImpLin / FacCPedi.TpoCmb.
                                                ELSE x-ImpLin = x-ImpLin * FacCPedi.TpoCmb.
                IF x-ImpMin <= x-ImpLin THEN x-Factor = TRUNCATE(x-ImpLin / x-ImpMin, 0).
            END.
            WHEN 2 THEN DO:     /* Por cantidades */
                x-CanDes = 0.
                FOR EACH Detalle:
                    IF Detalle.CanMin > 0 AND Detalle.CanMin > Detalle.CanPed THEN NEXT.
                    x-CanDes = x-CanDes + Detalle.CanPed.
                END.
                x-CanMin = Vtacprom.Cantidad.
                IF x-CanMin <= x-CanDes THEN x-Factor = TRUNCATE(x-CanDes / x-CanMin, 0).
            END.
            WHEN 3 THEN DO:     /* Por importes y proveedor  */
                x-ImpLin = 0.
                FOR EACH FacDPedi OF FacCPedi NO-LOCK, FIRST Almmmatg OF Facdpedi WHERE Almmmatg.codpr1 = VtaCProm.CodPro:
                    x-ImpLin = x-ImpLin + FacDPedi.ImpLin.
                END.
                IF FacCPedi.CodMon = 2 THEN x-ImpLin = x-ImpLin * FacCPedi.TpoCmb.
                x-Factor = 1.
            END.
        END CASE.
        IF x-Factor <= 0 THEN LEAVE PROMOCIONES.
        /* cargamos las promociones */
        CASE Vtacprom.TipProm:
            WHEN 1 OR WHEN 2 THEN DO:
                FOR EACH Vtadprom OF Vtacprom NO-LOCK WHERE Vtadprom.Tipo = 'G', FIRST Almmmatg OF Vtadprom NO-LOCK:
                    FIND Promocion WHERE Promocion.codmat = Vtadprom.codmat NO-ERROR.
                    IF NOT AVAILABLE Promocion THEN CREATE Promocion.
                    ASSIGN
                        Promocion.codcia = FacCPedi.codcia
                        Promocion.coddiv = FacCPedi.coddiv
                        Promocion.almdes = FacCPedi.codalm
                        Promocion.codmat = Vtadprom.codmat
                        Promocion.canped = Promocion.canped + ( Vtadprom.cantidad * x-Factor )
                        Promocion.undvta = Almmmatg.undbas
                        Promocion.aftigv = Almmmatg.AftIgv
                        Promocion.factor = 1.
                    IF Vtadprom.Tope > 0 AND Promocion.canped > Vtadprom.Tope
                        THEN Promocion.canped = Vtadprom.Tope.
                    ASSIGN
                        Promocion.Libre_c05 = 'OF'.          /* Marca de PROMOCION */
                END.
            END.
            WHEN 3 THEN DO:
                FOR EACH Vtadprom OF Vtacprom NO-LOCK WHERE Vtadprom.Tipo = 'G', FIRST Almmmatg OF Vtadprom NO-LOCK
                    BY Vtadprom.Importe DESC:
                    IF x-ImpLin >= Vtadprom.Importe THEN DO:
                        FIND Promocion WHERE Promocion.codmat = Vtadprom.codmat NO-ERROR.
                        IF NOT AVAILABLE Promocion THEN CREATE Promocion.
                        ASSIGN
                            Promocion.codcia = FacCPedi.codcia
                            Promocion.coddiv = FacCPedi.coddiv
                            Promocion.almdes = FacCPedi.codalm
                            Promocion.codmat = Vtadprom.codmat
                            Promocion.canped = Promocion.canped + ( Vtadprom.cantidad * x-Factor )
                            Promocion.undvta = Almmmatg.undbas
                            Promocion.aftigv = Almmmatg.AftIgv
                            Promocion.factor = 1.
                        IF Vtadprom.Tope > 0 AND Promocion.canped > Vtadprom.Tope
                            THEN Promocion.canped = Vtadprom.Tope.
                        ASSIGN
                            Promocion.Libre_c05 = 'OF'.          /* Marca de PROMOCION */
                        LEAVE PROMOCIONES.    /* <<< OJO <<< */
                    END.
                END.
            END.
        END CASE.
    END.
    /* AHORA ACUMULAMOS POR PROMOCION */
    FIND FIRST Promocion NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Promocion THEN DO:
        /* Borramos todo */
        FOR EACH ITEM OF Faccpedi WHERE ITEM.NroDoc = VtaCProm.NroDoc AND ITEM.Tipo = "PRODUCTO":
            DELETE ITEM.
        END.
    END.
    FOR EACH Promocion:
        CREATE ITEM.
        BUFFER-COPY Promocion TO ITEM
            ASSIGN
                ITEM.CodDiv = Faccpedi.coddiv
                ITEM.CodDoc = Faccpedi.coddoc
                ITEM.NroPed = Faccpedi.nroped
                ITEM.NroDoc = Vtacprom.nrodoc
                ITEM.Tipo   = "PROMOCION".
    END.
END.

END PROCEDURE.
                               
