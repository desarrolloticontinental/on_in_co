DEFINE TEMP-TABLE DETALLE LIKE wmigrv.
DEFINE TEMP-TABLE T-WMIGRV LIKE wmigrv.
DEFINE TEMP-TABLE T-WMIGCORR LIKE wmigcorr.

DEFINE VAR s-codcia  AS INTEGER INIT 001.
DEFINE VAR CB-CODCIA AS INTEGER INIT 000.
DEFINE VAR cl-codcia AS INTEGER INIT 000.
DEFINE VAR s-user-id AS CHAR INIT 'ADMIN'.

DEFINE VAR p-periodo AS INTE NO-UNDO.
DEFINE VAR p-mes     AS INTE NO-UNDO.
DEFINE VAR x-ctaigv  AS CHAR NO-UNDO.
DEFINE VAR x-ctadto  AS CHAR NO-UNDO.
DEFINE VAR x-ctaisc  AS CHAR NO-UNDO.
DEFINE VAR x-codope  AS CHAR NO-UNDO.
DEFINE VAR x-ctagan  AS CHAR NO-UNDO.
DEFINE VAR x-ctaper  AS CHAR NO-UNDO.
DEFINE VAR x-rndgan  AS CHAR NO-UNDO.
DEFINE VAR x-rndper  AS CHAR NO-UNDO.

DEFINE VAR FILL-IN-fchast AS DATE NO-UNDO.

DEFINE VARIABLE cRuc AS CHARACTER NO-UNDO.

DEFINE VARIABLE s-NroMesCie AS LOGICAL INITIAL YES.

FIND cb-cfgg WHERE cb-cfgg.CodCia = cb-codcia 
    AND cb-cfgg.Codcfg = 'R02' NO-LOCK NO-ERROR.
IF NOT AVAILABLE cb-cfgg THEN DO:
    MESSAGE 'NO está configurado R02' VIEW-AS ALERT-BOX ERROR.
    RETURN.
END.

DEF VAR FILL-IN-fchast-1 AS DATE.
DEF VAR FILL-IN-fchast-2 AS DATE.
DEF VAR f-Division AS CHAR.
DEF VAR x-Periodo AS CHAR INIT '2012'.
DEF VAR x-NroMes AS CHAR INIT '07'.
DEF VAR pOptions AS CHAR.
DEF VAR pArchivo AS CHAR.

ASSIGN
    FILL-IN-fchast-1 = 07/01/2012
    FILL-IN-fchast-2 = TODAY - 1.

EMPTY TEMP-TABLE DETALLE.
/* POR TODO EL MES */
FOR EACH gn-divi NO-LOCK WHERE gn-divi.codcia = s-codcia:
    f-Division = gn-divi.coddiv.
    FOR EACH FacDocum NO-LOCK WHERE FacDocum.CodCia = s-codcia 
        AND LOOKUP(FacDocum.CodDoc, 'FAC,BOL,TCK') > 0:
        IF FacDocum.CodCta[1] = '' THEN DO:
           MESSAGE 'Cuenta contable de ' + FacDocum.Coddoc + ' no configurada' VIEW-AS ALERT-BOX.
           NEXT.
        END.
        DO FILL-IN-fchast = FILL-IN-fchast-1 TO FILL-IN-fchast-2:
            RUN Carga-Fac-Bol.
        END.
    END.
END.
RUN transferir.
RUN d:/newsie/sie_co/on_in_co/aplic/lib/tt-file-to-text.r (OUTPUT pOptions, OUTPUT pArchivo).
IF pOptions = "" THEN RETURN NO-APPLY.
pOptions = pOptions + CHR(1) + "SkipList:FlagFecha,FlagMigracion,FlagTipo,FlagUsuario".
RUN d:/newsie/sie_co/on_in_co/aplic/lib/tt-file.r (TEMP-TABLE t-wmigrv:HANDLE, pArchivo, pOptions).




PROCEDURE Carga-Fac-Bol:

DEF VAR x-codcta AS CHAR.
DEF VAR x-codigv AS CHAR.
DEF VAR x-ctadto AS CHAR.
DEF VAR x-ctaisc AS CHAR.
DEF VAR x-ctaigv AS CHAR.
DEF VAR x-fchvto AS DATE.
DEF VAR x-coddoc AS CHAR.
DEF VAR x-nrodoc AS CHAR.
DEF VAR x-codcli AS CHAR.
DEF VAR x-codmon AS INT.
DEF VAR x-fchdoc AS INT.

DEFINE VAR x-detalle AS LOGICAL NO-UNDO.
DEFINE VAR x-cco     AS CHAR    NO-UNDO.

/* CARGAMOS LA INFORMACION EN EL TEMPORAL */
FIND cb-cfgg WHERE cb-cfgg.CodCia = cb-codcia 
    AND cb-cfgg.Codcfg = 'R02' NO-LOCK NO-ERROR.
IF NOT AVAILABLE cb-cfgg THEN RETURN.

FOR EACH CcbCDocu NO-LOCK WHERE CcbCDocu.codcia = s-codcia 
    AND CcbCDocu.Coddiv = f-Division 
    AND CcbCDocu.Tipo = "MOSTRADOR"
    AND CcbCDocu.FchDoc = FILL-IN-fchast 
    AND CcbCDocu.CodDoc = FacDocum.CodDoc:
    /* PARCHE: Boletas anuladas NO deben pasar */
    IF Ccbcdocu.coddoc = 'BOL' AND Ccbcdocu.flgest = 'A' THEN NEXT.
    /* *************************************** */
    FIND gn-clie WHERE gn-clie.codcia = cl-codcia
        AND gn-clie.codcli = ccbcdocu.codcli
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE gn-clie THEN RETURN.
    FIND TabDistr WHERE TabDistr.CodDepto = gn-clie.coddep
        AND TabDistr.CodProvi = gn-clie.codprov
        AND TabDistr.CodDistr = gn-clie.coddist
        NO-LOCK NO-ERROR.
    FIND Cb-cfgrv WHERE Cb-cfgrv.Codcia = Ccbcdocu.Codcia 
        AND Cb-cfgrv.CodDiv = Ccbcdocu.Coddiv 
        AND Cb-cfgrv.Coddoc = Ccbcdocu.Coddoc 
        AND Cb-cfgrv.Fmapgo = Ccbcdocu.Fmapgo 
        AND Cb-cfgrv.Codmon = Ccbcdocu.Codmon 
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Cb-cfgrv OR Cb-cfgrv.Codcta = "" 
        THEN ASSIGN
                x-detalle = YES
                x-codcta = (IF ccbcdocu.codmon = 1 THEN FacDocum.CodCta[1] ELSE FacDocum.CodCta[2]).
        ELSE ASSIGN
                x-detalle = Cb-cfgrv.Detalle
                x-codcta = Cb-cfgrv.codcta.
    ASSIGN
        x-ctaisc = cb-cfgg.codcta[2] 
        x-ctaigv = cb-cfgg.codcta[3]
        x-ctadto = cb-cfgg.codcta[10]
        x-fchvto = (IF ccbcdocu.fchvto < ccbcdocu.fchdoc THEN ccbcdocu.fchdoc ELSE ccbcdocu.fchvto)
        x-coddoc = Ccbcdocu.coddoc
        x-nrodoc = Ccbcdocu.nrodoc
        x-codcli = Ccbcdocu.codcli
        x-codmon = (IF ccbcdocu.codmon = 1 THEN 00 ELSE 01)
        x-fchdoc = YEAR(ccbcdocu.fchdoc) * 10000 + MONTH(ccbcdocu.fchdoc) * 100 + DAY(ccbcdocu.fchdoc).
    CASE ccbcdocu.coddoc:
        WHEN 'FAC' THEN x-coddoc = "FC".
        WHEN 'BOL' THEN x-coddoc = "BV".
        WHEN 'TCK' THEN x-coddoc = "TK".
        WHEN 'LET' THEN x-coddoc = "LT".
        WHEN 'N/C' THEN x-coddoc = "NC".
        WHEN 'N/D' THEN x-coddoc = "ND".
        WHEN 'CHQ' THEN x-coddoc = "CD".
    END CASE.
    IF x-Detalle = NO THEN DO:
        /* UN SOLO NUMERO DE DOCUMENTO (SIN DETALLE) */
        ASSIGN
            X-NRODOC = SUBSTRING(Ccbcdocu.Nrodoc,1,3) + "111111"
            X-CODCLI = '1111111111'.
    END.
    ELSE DO:
        x-codcli = (IF gn-clie.codant = '' THEN SUBSTRING(gn-clie.codcli,1,10) ELSE gn-clie.codant).
    END.
    IF Ccbcdocu.FlgEst = "A" THEN x-codcta = (IF Ccbcdocu.Coddoc = "FAC" THEN "12122100" ELSE "12121140").

    FIND DETALLE WHERE DETALLE.wvtdoc = x-coddoc
        AND DETALLE.wvndoc = x-nrodoc
        AND DETALLE.wvmone = x-codmon
        AND DETALLE.wvcpvt = x-codcta
        AND DETALLE.wvfech = x-FchDoc
        NO-ERROR.
    IF NOT AVAILABLE DETALLE THEN CREATE DETALLE.
    ASSIGN
        DETALLE.FlagTipo = "I"
        DETALLE.FlagUsuario = s-user-id.
    ASSIGN
        DETALLE.wsecue = 0001
        DETALLE.wvejer = YEAR(ccbcdocu.fchdoc)
        DETALLE.wvperi = MONTH(ccbcdocu.fchdoc)
        DETALLE.wvtdoc = x-CodDoc
        DETALLE.wvndoc = x-NroDoc
        DETALLE.wvfech = x-FchDoc
        DETALLE.wvccli = x-codcli
        DETALLE.wvclie = SUBSTRING (gn-clie.nomcli, 1, 40)
        DETALLE.wvcdir = (IF gn-clie.dircli <> '' THEN SUBSTRING (gn-clie.dircli, 1, 40) ELSE 'LIMA')
        DETALLE.wvcdis = (IF AVAILABLE TabDistr THEN SUBSTRING (TabDistr.NomDistr, 1, 40) ELSE 'LIMA')
        DETALLE.wvref3 = (IF ccbcdocu.codcli BEGINS '2' THEN "PJ" ELSE "NI")
        DETALLE.wvtido = (IF ccbcdocu.codcli BEGINS '2' THEN "RU" ELSE "")
        DETALLE.wvnudo = (IF ccbcdocu.codcli BEGINS '2' THEN gn-clie.ruc ELSE SUBSTRING(gn-clie.codcli, 3, 8))
        DETALLE.wvmone = x-codmon
        DETALLE.wvtcam = 0.00.            /* ccbcdocu.tpocmb. */
    IF Ccbcdocu.flgest = "A" THEN DO:       /* ANULADO */
        ASSIGN
            DETALLE.wvpvta = 0.00
            DETALLE.wvcpvt = x-codcta
            DETALLE.wvmpvt = (IF ccbcdocu.coddoc = "N/C" THEN "A" ELSE "C").
        ASSIGN
            DETALLE.wvfepr = YEAR(TODAY) * 10000 + MONTH(TODAY) * 100 + DAY(TODAY)
            DETALLE.wvfeve = YEAR(x-fchvto) * 10000 + MONTH(x-fchvto) * 100 + DAY(x-fchvto)
            DETALLE.wvruc = ccbcdocu.ruccli
            DETALLE.wvndom = (IF DETALLE.wvruc = '' THEN "N" ELSE (IF DETALLE.wvref3 = "PJ" THEN "S" ELSE "N"))
            DETALLE.wvcpag = ccbcdocu.fmapgo
            DETALLE.wvsitu = "01"        /* NO Graba en Cuentas por Cobrar */
            DETALLE.wvcost = "9999999"
            DETALLE.wvcven = ccbcdocu.codven
            DETALLE.wvacti = ccbcdocu.coddiv
            DETALLE.wvnbco = ''  /* ver ticketeras */
            DETALLE.wvusin = ccbcdocu.usuario
            DETALLE.wvfein = YEAR(TODAY) * 10000 + MONTH(TODAY) * 100 + DAY(TODAY).
        NEXT.
    END.
    IF ccbcdocu.impbrt > 0 
        THEN ASSIGN
                DETALLE.wvvalv = DETALLE.wvvalv + CcbCDocu.ImpBrt
                DETALLE.wvcval = cb-cfgg.codcta[5]
                DETALLE.wvmval = (IF ccbcdocu.coddoc = "N/C" THEN "C" ELSE "A").
    IF ccbcdocu.impexo > 0 
        THEN ASSIGN
                DETALLE.wvvali = DETALLE.wvvali + ccbcdocu.impexo
                DETALLE.wvcvai = cb-cfgg.codcta[6]
                DETALLE.wvmvai = ( IF ccbcdocu.coddoc = "N/C" THEN "C" ELSE "A" ).
    IF ccbcdocu.impdto > 0 
        THEN ASSIGN
                DETALLE.wvdsct = DETALLE.wvdsct + ccbcdocu.impdto
                DETALLE.wvcdsc = x-ctadto
                DETALLE.wvmdsc = (IF ccbcdocu.coddoc = "N/C" THEN "A" ELSE "C").
    IF ccbcdocu.impigv > 0 
        THEN ASSIGN
                DETALLE.wvigv = DETALLE.wvigv + ccbcdocu.impigv
                DETALLE.wvcigv = x-ctaigv
                DETALLE.wvmigv = (IF ccbcdocu.coddoc = "N/C" THEN "C" ELSE "A").
    ASSIGN
        DETALLE.wvpvta = DETALLE.wvpvta + ccbcdocu.imptot
        DETALLE.wvcpvt = x-codcta
        DETALLE.wvmpvt = (IF ccbcdocu.coddoc = "N/C" THEN "A" ELSE "C").
    IF ccbcdocu.coddoc = 'N/C' OR ccbcdocu.coddoc = 'N/D' THEN DO:
        CASE ccbcdocu.codref:
            WHEN 'FAC' THEN DETALLE.wvtref = "FC".
            WHEN 'BOL' THEN DETALLE.wvtref = "BV".
            WHEN 'TCK' THEN DETALLE.wvtref = "TK".
            WHEN 'LET' THEN DETALLE.wvtref = "LT".
            WHEN 'N/C' THEN DETALLE.wvtref = "NC".
            WHEN 'N/D' THEN DETALLE.wvtref = "ND".
            WHEN 'CHQ' THEN DETALLE.wvtref = "CD".
        END CASE.
        ASSIGN
            DETALLE.wvnref = ccbcdocu.nroref.
    END.
    ELSE DO:
        IF Ccbcdocu.codped = "PED" 
            THEN ASSIGN
                    DETALLE.wvtref = "PD"
                    DETALLE.wvnref = Ccbcdocu.nroped.
    END.
    ASSIGN
        DETALLE.wvfepr = YEAR(TODAY) * 10000 + MONTH(TODAY) * 100 + DAY(TODAY)
        DETALLE.wvfeve = YEAR(x-fchvto) * 10000 + MONTH(x-fchvto) * 100 + DAY(x-fchvto)
        DETALLE.wvruc = ccbcdocu.ruccli
        DETALLE.wvndom = (IF DETALLE.wvruc = '' THEN "N" ELSE (IF DETALLE.wvref3 = "PJ" THEN "S" ELSE "N"))
        DETALLE.wvcpag = ccbcdocu.fmapgo
        DETALLE.wvsitu = "01"        /* NO Graba en Cuentas por Cobrar */
        DETALLE.wvcost = "9999999"
        DETALLE.wvcven = ccbcdocu.codven
        DETALLE.wvacti = ccbcdocu.coddiv
        DETALLE.wvnbco = ''  /* ver ticketeras */
        DETALLE.wvusin = ccbcdocu.usuario
        DETALLE.wvfein = YEAR(TODAY) * 10000 + MONTH(TODAY) * 100 + DAY(TODAY).
END.    


END PROCEDURE.


PROCEDURE Transferir:

  DEFINE VARIABLE OK-WAIT-STATE AS LOGICAL NO-UNDO.
  
  OK-WAIT-STATE = SESSION:SET-WAIT-STATE("GENERAL").  
  /* BLOQUEAMOS Sí o Sí el correlativo */
  REPEAT:
      FIND T-WMIGCORR WHERE T-WMIGCORR.Proceso = "RV"
          AND T-WMIGCORR.Periodo = INTEGER(x-Periodo)
          AND T-WMIGCORR.Mes = INTEGER(x-NroMes)
          EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
      IF NOT AVAILABLE T-WMIGCORR THEN DO:
          IF NOT LOCKED T-WMIGCORR THEN DO:
              /* CREAMOS EL CONTROL */
              CREATE T-WMIGCORR.
              ASSIGN
                  T-WMIGCORR.Correlativo = 1
                  T-WMIGCORR.Mes = INTEGER(x-NroMes)
                  T-WMIGCORR.Periodo = INTEGER(x-Periodo)
                  T-WMIGCORR.Proceso = "RV".
              LEAVE.
          END.
          ELSE UNDO, RETRY.
      END.
      LEAVE.
  END.
  FOR EACH DETALLE NO-LOCK:
      CREATE T-WMIGRV.
      BUFFER-COPY DETALLE
          TO T-WMIGRV
          ASSIGN
          T-WMIGRV.FlagFecha = DATETIME(TODAY, MTIME)
          T-WMIGRV.FlagTipo = "I"
          T-WMIGRV.FlagUsuario = s-user-id
          T-WMIGRV.wcorre = STRING(T-WMIGCORR.Periodo, '9999') + STRING(T-WMIGCORR.Mes, '99') + 
                          STRING(T-WMIGCORR.Correlativo, '9999')
          T-WMIGRV.wsecue = 0001
          T-WMIGRV.wvejer = INTEGER(x-Periodo)
          T-WMIGRV.wvperi = INTEGER(x-NroMes).
    ASSIGN
        t-wmigcorr.Correlativo = t-wmigcorr.Correlativo +  1.
  END.
  EMPTY TEMP-TABLE DETALLE.
  OK-WAIT-STATE = SESSION:SET-WAIT-STATE("").

END PROCEDURE.
