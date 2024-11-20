/* AJUSTES DE INVENTARIOS POR TRANSFERENCIAS */
DEF VAR x-linea AS CHAR FORMAT 'x(100)'.
DEF VAR x-compania AS CHAR INIT 'CONTINENTAL'.   /* <<<< OJO <<<< */
DEF VAR x-codmov AS INT INIT 03.    /* TRANSFERENCIAS */
DEF VAR s-codcia AS INT INIT 001.
DEF VAR s-user-id AS CHAR INIT 'SISTEMAS'.
DEF VAR x-Glosa AS CHAR.
DEF VAR x-NroItm AS INT.

DEF VAR x-nroser LIKE almcmov.nroser.
DEF VAR x-nrodoc LIKE almcmov.nrodoc.

DISABLE TRIGGERS FOR LOAD OF almcmov.
DISABLE TRIGGERS FOR LOAD OF almdmov.
DISABLE TRIGGERS FOR LOAD OF almtdocm.

DEF TEMP-TABLE detalle
    FIELD compania AS CHAR FORMAT 'x(15)'
    FIELD codalm   AS CHAR FORMAT 'x(10)'
    FIELD tipmov   AS CHAR FORMAT 'x(10)'
    FIELD codmat   AS CHAR FORMAT 'x(6)'
    FIELD cantidad AS DEC FORMAT '>>>,>>>,>>9.99'.

DEF TEMP-TABLE ITEM LIKE detalle
    FIELD codcia AS INT
    FIELD codmov LIKE almdmov.codmov
    INDEX llave01 AS PRIMARY compania codalm tipmov codmat.

INPUT FROM c:\tmp\ajuste.prn.
REPEAT :
    CREATE detalle.
    IMPORT detalle.
    FIND ITEM WHERE ITEM.compania = detalle.compania
        AND ITEM.codalm = detalle.codalm
        AND ITEM.tipmov = detalle.tipmov
        AND ITEM.codmat = detalle.codmat
        NO-ERROR.
    /* ARCHIVO RESUMIDO */
    IF NOT AVAILABLE ITEM THEN CREATE ITEM.
    BUFFER-COPY detalle 
        EXCEPT detalle.cantidad
        TO ITEM
        ASSIGN
            ITEM.codcia = s-codcia
            ITEM.codmov = x-codmov.
    ASSIGN ITEM.cantidad = ITEM.cantidad + detalle.cantidad.
END.
INPUT CLOSE.

DO TRANSACTION ON ERROR UNDO, RETURN ERROR 
    ON STOP UNDO, RETURN ERROR:
    FOR EACH ITEM NO-LOCK WHERE ITEM.compania = x-compania
        AND ITEM.codmat <> '',
        FIRST Almmmatg OF ITEM NO-LOCK
        BREAK BY ITEM.codalm BY ITEM.tipmov:
        IF FIRST-OF(ITEM.codalm) OR FIRST-OF(ITEM.tipmov) THEN DO:
            /* Cabecera */
            CASE ITEM.TipMov:
                WHEN "I" THEN DO:
                    FIND Almtdocm OF ITEM NO-ERROR.
                    IF NOT AVAILABLE Almtdocm
                    THEN DO:
                        MESSAGE 'NO esta configurado el movimiento' ITEM.tipmov ITEM.codmov 'en el almacen' ITEM.codalm
                            VIEW-AS ALERT-BOX ERROR.
                        UNDO, RETURN ERROR.
                    END.
                END.
                WHEN "S" THEN DO:
                    /* Buscamos el correlativo de almacenes */
                    FIND Almacen WHERE Almacen.CodCia = S-CODCIA 
                        AND Almacen.CodAlm = ITEM.CodAlm 
                        NO-ERROR.
                    IF NOT AVAILABLE Almacen THEN DO: 
                        MESSAGE 'NO se pudo bloquer el correlativo por almacen' VIEW-AS ALERT-BOX ERROR.
                        UNDO, RETURN 'ADM-ERROR'.
                    END.
                END.
            END CASE.
            X-Glosa = 'AjusteAutomaticoxInve' + STRING(TODAY,'99/99/99') + STRING(TIME,'HH:MM:SS') + s-User-id.
            CREATE Almcmov.
            ASSIGN 
                Almcmov.CodCia = s-CodCia
                Almcmov.CodAlm = ITEM.CodAlm                        
                Almcmov.TipMov = ITEM.TipMov
                Almcmov.CodMov = ITEM.CodMov
                Almcmov.Observ = X-Glosa
                Almcmov.Usuario = s-User-id
                Almcmov.FchDoc = TODAY
                Almcmov.CodMon = 1
                Almcmov.TpoCmb = 0.
            ASSIGN
                x-nroser = almcmov.nroser
                x-nrodoc = almcmov.nrodoc
                x-NroItm = 1.
            CASE ITEM.TipMov:
                WHEN "I" THEN DO:
                    ASSIGN
                        Almcmov.NroDoc = AlmtDocm.NroDoc
                        AlmtDocm.NroDoc = AlmtDocm.NroDoc + 1.
                END.
                WHEN "S" THEN DO:
                    ASSIGN
                        Almcmov.NroDoc = Almacen.CorrSal
                        Almacen.CorrSal = Almacen.CorrSal + 1.
                END.
            END CASE.
        END.
        CREATE Almdmov.
        ASSIGN
            Almdmov.CodCia = s-codcia
            Almdmov.CodAlm = item.codalm
            Almdmov.TipMov = item.TipMov
            Almdmov.CodMov = item.CodMov
            Almdmov.NroSer = x-NroSer
            Almdmov.NroDoc = x-NroDoc
            Almdmov.FchDoc = TODAY
            Almdmov.TpoCmb = 0
            Almdmov.CodMon = 1
            Almdmov.codmat = ITEM.codmat
            Almdmov.CanDes = ITEM.cantidad
            Almdmov.CodUnd = Almmmatg.UndStk
            Almdmov.Factor = 1
            Almdmov.Flg_Factor = "T"
            Almdmov.NroItm = x-NroItm.
        x-NroItm = x-NroItm + 1.
    END.
END.
