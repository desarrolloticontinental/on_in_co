DEF NEW SHARED VAR s-codcia AS INT INIT 001.
DEF NEW SHARED VAR s-coddiv AS CHAR.
DEF NEW SHARED VAR s-codalm AS CHAR.

DEF VAR s-coddoc AS CHAR INIT 'COT' NO-UNDO.
DEF VAR s-nroser AS INT NO-UNDO.

DEF BUFFER b-cpedi FOR faccpedi.
DEF BUFFER b-dpedi FOR facdpedi.
DEF TEMP-TABLE ITEM LIKE FacDPedi.

DEF VAR x-Vuelta AS INT NO-UNDO.
DEF VAR I-NITEM AS INTEGER NO-UNDO.
DEF VAR x-Rowid AS ROWID NO-UNDO.

RLOOP:
FOR EACH b-cpedi WHERE b-cpedi.codcia = s-codcia
    AND LOOKUP(b-cpedi.coddiv, '00015,00018') > 0
    AND b-cpedi.coddoc = s-coddoc
    AND LOOKUP(b-cpedi.libre_c01, '00015,20015') > 0
    AND b-cpedi.fchped >= DATE(09,28,2016)
    AND b-cpedi.fchped <= DATE(10,25,2016)
    AND b-cpedi.flgest = 'P':
    DISPLAY coddiv libre_c01 coddoc nroped fchped WITH STREAM-IO NO-BOX.
    PAUSE 0.
    ASSIGN
        s-coddiv = b-cpedi.coddiv
        s-nroser = INTEGER(SUBSTRING(b-cpedi.nroped,1,3))
        s-codalm = b-cpedi.codalm.
    /* Podemos dividirla hasta en 3 cotizaciones */
    EMPTY TEMP-TABLE ITEM.
    FOR EACH b-dpedi OF b-cpedi NO-LOCK:
        CREATE ITEM.
        BUFFER-COPY b-dpedi TO ITEM.
    END.
    DO x-Vuelta = 1 TO 3:
        x-Rowid = ?.
        I-NITEM = 0.
        CASE x-Vuelta:
            WHEN 1 THEN DO:
                FOR EACH ITEM NO-LOCK,
                    FIRST Almmmatg OF ITEM NO-LOCK WHERE Almmmatg.codfam = '010'
                    AND LOOKUP(Almmmatg.subfam, '001,012,014') > 0:
                    RUN Crea-Cabecera.
                    IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO RLOOP, RETURN 'ADM-ERROR'.
                    LEAVE.
                END.
                IF x-Rowid <> ? THEN DO:
                    FIND Faccpedi WHERE ROWID(Faccpedi) = x-Rowid.
                    FOR EACH ITEM,
                        FIRST Almmmatg OF ITEM NO-LOCK WHERE Almmmatg.codfam = '010'
                        AND LOOKUP(Almmmatg.subfam, '001,012,014') > 0
                        BY ITEM.nroitm:
                        RUN Crea-Detalle.
                    END.
                    RUN Graba-Totales.
                END.
            END.
            WHEN 2 THEN DO:
                FOR EACH ITEM NO-LOCK,
                    FIRST Almmmatg OF ITEM NO-LOCK WHERE Almmmatg.codfam = '011':
                    RUN Crea-Cabecera.
                    IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO RLOOP, RETURN 'ADM-ERROR'.
                    LEAVE.
                END.
                IF x-Rowid <> ? THEN DO:
                    FIND Faccpedi WHERE ROWID(Faccpedi) = x-Rowid.
                    FOR EACH ITEM, FIRST Almmmatg OF ITEM NO-LOCK WHERE Almmmatg.codfam = '011'
                        BY ITEM.nroitm:
                        RUN Crea-Detalle.
                    END.
                    RUN Graba-Totales.
                END.
            END.
            WHEN 3 THEN DO:
                FOR EACH ITEM NO-LOCK:
                    RUN Crea-Cabecera.
                    IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO RLOOP, RETURN 'ADM-ERROR'.
                    LEAVE.
                END.
                IF x-Rowid <> ? THEN DO:
                    FIND Faccpedi WHERE ROWID(Faccpedi) = x-Rowid.
                    FOR EACH ITEM BY ITEM.nroitm:
                        RUN Crea-Detalle.
                    END.
                    RUN Graba-Totales.
                END.
            END.
        END CASE.
    END.
    ASSIGN
        b-cpedi.flgest = 'X'.   /* CERRADO MANUALMENTE */
END.

PROCEDURE Crea-Cabecera:
/* ******************** */

    CREATE Faccpedi.
    {vta2/icorrelativosecuencial.i &Codigo = s-coddoc &Serie = s-nroser}
    BUFFER-COPY b-cpedi TO Faccpedi
        ASSIGN 
        FacCPedi.FchPed = TODAY 
        FacCPedi.Hora   = STRING(TIME, 'HH:MM:SS')
        FacCPedi.NroPed = STRING(FacCorre.NroSer,"999") + STRING(FacCorre.Correlativo,"999999")
        FacCPedi.CodRef = b-cpedi.coddoc
        FacCPedi.NroRef = b-cpedi.nroped.
    ASSIGN
        FacCorre.Correlativo = FacCorre.Correlativo + 1.
    x-Rowid = ROWID(Faccpedi).

END PROCEDURE.

PROCEDURE Crea-Detalle:
/* ******************* */

    I-NITEM = I-NITEM + 1.
    CREATE Facdpedi.
    BUFFER-COPY ITEM TO Facdpedi
        ASSIGN
        FacDPedi.CodCia = FacCPedi.CodCia
        FacDPedi.CodDiv = FacCPedi.CodDiv
        FacDPedi.coddoc = FacCPedi.coddoc
        FacDPedi.NroPed = FacCPedi.NroPed
        FacDPedi.FchPed = FacCPedi.FchPed
        FacDPedi.Hora   = FacCPedi.Hora 
        FacDPedi.FlgEst = FacCPedi.FlgEst
        FacDPedi.NroItm = I-NITEM.
    DELETE ITEM.

END PROCEDURE.

PROCEDURE Graba-Totales:
/* ******************** */

    {vta2/graba-totales-cotizacion-cred.i}

END.
