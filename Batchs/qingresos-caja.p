&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE SHARED VAR s-codcia AS INT.
DEFINE VAR lFechaProceso AS DATE .

lFechaProceso = 02/15/2016 /*TODAY - 1.*/.

DEFINE TEMP-TABLE tt-caja
    FIELDS fchproc      AS DATE                               COLUMN-LABEL "FECHA"     
    FIELDS canal        AS CHAR     FORMAT 'x(15)'            COLUMN-LABEL "CANAL VTA"         
    FIELDS coddiv       AS CHAR     FORMAT 'x(6)'             COLUMN-LABEL "DIVISION"         
    FIELDS desdiv       AS CHAR     FORMAT 'x(50)'            COLUMN-LABEL "NOMBRE"
    FIELDS codmon       AS CHAR     FORMAT 'x(6)'             COLUMN-LABEL "MONEDA"
    FIELDS TipoVta      AS CHAR     FORMAT 'x(10)'            COLUMN-LABEL "TIPO VTA"     
    /*
    FIELDS impcred      AS DEC      FORMAT '-ZZ,ZZZ,ZZ9.99'   COLUMN-LABEL "TOTAL CREDITO"         INIT 0.00
    FIELDS impcontado   AS DEC      FORMAT '-ZZ,ZZZ,ZZ9.99'   COLUMN-LABEL "TOTAL CONTADO"   INIT 0.00
    */
    FIELDS impmnorig    AS DEC      FORMAT '-ZZ,ZZZ,ZZ9.99'   COLUMN-LABEL "MONEDA ORIGINAL"   INIT 0.00
    FIELDS imptotal     AS DEC      FORMAT '-ZZ,ZZZ,ZZ9.99'   COLUMN-LABEL "IMPTE VENTA"   INIT 0.00
    FIELDS impcaja      AS DEC      FORMAT '-ZZ,ZZZ,ZZ9.99'   COLUMN-LABEL "IMPTE COBRADO"   INIT 0.00
    FIELDS impefe       AS DEC      FORMAT '-ZZ,ZZZ,ZZ9.99'   COLUMN-LABEL "EFECTIVO"        INIT 0.00
    FIELDS impn-c       AS DEC      FORMAT '-ZZ,ZZZ,ZZ9.99'   COLUMN-LABEL "N/C"             INIT 0.00
    FIELDS impchqdia    AS DEC      FORMAT '-ZZ,ZZZ,ZZ9.99'   COLUMN-LABEL "CHEQUE DEL DIA"  INIT 0.00
    FIELDS impchqdif    AS DEC      FORMAT '-ZZ,ZZZ,ZZ9.99'   COLUMN-LABEL "CHEQUE DIFERIDO" INIT 0.00
    FIELDS impcard      AS DEC      FORMAT '-ZZ,ZZZ,ZZ9.99'   COLUMN-LABEL "TARJETA"         INIT 0.00
    FIELDS impbd        AS DEC      FORMAT '-ZZ,ZZZ,ZZ9.99'   COLUMN-LABEL "BOLETA DEPOSITO" INIT 0.00
    FIELDS impar        AS DEC      FORMAT '-ZZ,ZZZ,ZZ9.99'   COLUMN-LABEL "ANTICIPOS"       INIT 0.00
    FIELDS impvales     AS DEC      FORMAT '-ZZ,ZZZ,ZZ9.99'   COLUMN-LABEL "VALES CONSUMO"   INIT 0.00
    INDEX idx01 IS PRIMARY fchproc coddiv codmon tipovta.
    
    /*FIELDS impvuelto    AS DEC      FORMAT 'ZZ,ZZZ,ZZ9.99'   COLUMN-LABEL "VUELTO"          INIT 0.00*/.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

RUN pventas-caja.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-pventas-caja) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pventas-caja Procedure 
PROCEDURE pventas-caja :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE VAR lMoneda AS CHAR.
DEFINE VAR lDocs AS CHAR.
DEFINE VAR lDoc AS CHAR.
DEFINE VAR lSec AS INT.
DEFINE VAR lIni AS DATETIME.
DEFINE VAR lFin AS DATETIME.
DEFINE VAR lEsCredito AS LOG.

DEFINE VAR lSumaCaja AS DEC.
DEFINE VAR lFactor AS DEC.
DEFINE VAR limpte AS DEC.
DEFINE VAR lTipoCmb AS DEC.
DEFINE VAR lTipoVta AS CHAR.
DEFINE VAR lCanal AS CHAR.

lDocs = 'FAC,BOL,TCK,N/C'.
lIni = NOW.

EMPTY TEMP-TABLE tt-caja.

SESSION:SET-WAIT-STATE('GENERAL').

DEFINE BUFFER ix-ccbcdocu FOR ccbcdocu.

/* DOCUMENTOS */
REPEAT lSec = 1 TO NUM-ENTRIES(lDocs,","):
    lDoc = ENTRY(lSec,lDocs,",").
    /*FOR EACH gn-div NO-LOCK:*/
        FOR EACH ccbcdocu USE-INDEX llave13 WHERE ccbcdocu.codcia = s-codcia AND 
                                ccbcdocu.fchdoc = lFechaProceso AND
                                ccbcdocu.coddoc = lDoc AND
                                ccbcdocu.flgest <> 'A' AND 
                                (ccbcdocu.tpofac <> 'A' AND ccbcdocu.tpofac <> 'S')     /* No incluye Anticipos ni Servicios */
                                NO-LOCK:

            /* Buscamos el Tipo de Venta */ 
            lTipoVta = ccbcdocu.tipo.
            lFactor = 1.
            lCanal  = "<SIN CANAL>".
            IF ccbcdocu.coddoc = 'N/C' THEN DO:
                /* Lo sacamos del documento de referencia */
                FIND FIRST ix-ccbcdocu USE-INDEX llave01 WHERE ix-ccbcdocu.codcia = s-codcia AND 
                                            ix-ccbcdocu.coddoc = ccbcdocu.codref AND
                                            ix-ccbcdocu.nrodoc = ccbcdocu.nroref
                                            NO-LOCK NO-ERROR.
                IF AVAILABLE ix-ccbcdocu THEN DO:
                    lTipoVta = ix-ccbcdocu.tipo.
                END.
                lFactor = -1.
            END.
            /* Por la Forma de Pago */
            FIND FIRST gn-convt WHERE gn-convt.codig = ccbcdocu.fmapgo NO-LOCK NO-ERROR.
            IF AVAILABLE gn-convt AND gn-convt.tipvta = '1' THEN DO:
                lTipoVta = "MOSTRADOR".
            END.

            FIND FIRST gn-div WHERE gn-div.codcia = s-codcia AND
                                    gn-div.coddiv = ccbcdocu.divori
                                    NO-LOCK NO-ERROR.

            IF AVAILABLE gn-div THEN DO:
                FIND FIRST vtamcanal WHERE vtamcanal.codcia = s-codcia AND
                                            vtamcanal.canalventa = gn-divi.canalventa
                                            NO-LOCK NO-ERROR.
                IF AVAILABLE vtamcanal THEN lCanal = TRIM(vtamcanal.descrip).
            END.

            lTipoCmb  = 1.
            lMoneda = IF(ccbcdocu.codmon = 1) THEN "SOLES" ELSE "USD".
            lTipoVta = IF(lTipoVta = "MOSTRADOR") THEN "CONTADO" ELSE lTipoVta.
            lTipoCmb = IF(ccbcdocu.codmon = 1) THEN 1 ELSE ccbcdocu.tpocmb.

            FIND FIRST tt-caja WHERE    tt-caja.fchproc = lFechaProceso AND
                                        tt-caja.coddiv = ccbcdocu.divori AND
                                        tt-caja.codmon = lMoneda AND
                                        tt-caja.tipovta = lTipoVta
                                        NO-ERROR.
            IF NOT AVAILABLE tt-caja THEN DO:
                CREATE tt-caja.
                    ASSIGN  tt-caja.fchproc = lFechaProceso
                            tt-caja.coddiv  = ccbcdocu.divori
                            tt-caja.codmon  = lMoneda
                            tt-caja.tipovta = lTipoVta
                            tt-caja.canal   = lCanal
                            tt-caja.desdiv  = gn-div.desdiv.
            END.
            /* Diferente de Soles */
            IF ccbcdocu.codmon <> 1 THEN DO:
                ASSIGN  tt-caja.impmnorig = tt-caja.impmnorig + (ccbcdocu.imptot * lFactor).
            END.

            IF lTipoVta = 'MOSTRADOR' THEN DO:
                /* CONTADO */
                ASSIGN  tt-caja.imptot = tt-caja.imptot + (ccbcdocu.imptot * lFactor * lTipoCmb).
            END.
            ELSE DO:
                /* CREDITO */
                ASSIGN  tt-caja.imptot = tt-caja.imptot + (ccbcdocu.imptot * lFactor * lTipoCmb).
            END.

            IF ccbcdocu.coddoc = 'N/C' THEN DO:
                /* Regresamos */ 
                NEXT.
            END.

            /* CONTADOS CANCELACIONES */

            IF AVAILABLE gn-convt AND gn-convt.tipvta = '1' THEN DO:

                /* Buscamos el pago de caja */
                FIND FIRST ccbdcaja USE-INDEX llave02 WHERE ccbdcaja.codcia = s-codcia AND
                                            ccbdcaja.codref = ccbcdocu.coddoc AND
                                            ccbdcaja.nroref = ccbcdocu.nrodoc NO-LOCK NO-ERROR.
                IF AVAILABLE ccbdcaja AND ccbdcaja.fchdoc = lFechaProceso THEN DO:                    
                    /* Voy al Header de la Caja */
                    FIND FIRST ccbccaja OF ccbdcaja NO-LOCK NO-ERROR.
                    IF AVAILABLE ccbccaja THEN DO:
                        
                        /* Total del pago de la CAJA */
                        DEFINE BUFFER ix-ccbdcaja FOR ccbdcaja.                        
                        lSumaCaja = 0.
                        FOR EACH ix-ccbdcaja OF ccbccaja NO-LOCK:
                            IF ix-ccbdcaja.codref = ccbcdocu.coddoc AND ix-ccbdcaja.nroref = ccbcdocu.nrodoc THEN DO:
                                /* El importe cancelado del documento (puede ser parcial) */
                                limpte = ix-ccbdcaja.imptot.
                            END.                            
                            lSumaCaja = lSumaCaja + ix-ccbdcaja.imptot.
                        END.
                        RELEASE ix-ccbdcaja.
                        /* El factor con respecto al TOTAL */
                        IF limpte >= lSumaCaja THEN DO:
                            lFactor = 1.
                        END.
                        ELSE DO:
                            lFactor = lImpte / lSumaCaja.
                        END.                        

                        lMoneda = IF(ccbccaja.codmon = 1) THEN "SOLES" ELSE "USD".
                        FIND FIRST tt-caja WHERE    tt-caja.fchproc = lFechaProceso AND
                                                    tt-caja.coddiv = ccbcdocu.divori AND
                                                    tt-caja.codmon = lMoneda AND
                                                    tt-caja.tipovta = lTipoVta
                                                    NO-ERROR.
                        IF NOT AVAILABLE tt-caja THEN DO:
                            CREATE tt-caja.
                                ASSIGN  tt-caja.fchproc = lFechaProceso
                                        tt-caja.coddiv  = ccbcdocu.divori
                                        tt-caja.codmon  = lMoneda
                                        tt-caja.tipovta = lTipoVta
                                        tt-caja.desdiv  = IF(AVAILABLE gn-div) THEN gn-div.desdiv ELSE "".
                        END.
                        ASSIGN  tt-caja.impefe      = tt-caja.impefe + IF(lMoneda = 'SOLES') THEN ccbccaja.impnac[1] * lFactor ELSE ccbccaja.impusa[1] * lFactor
                                tt-caja.impn-c      = tt-caja.impn-c + IF(lMoneda = 'SOLES') THEN ccbccaja.impnac[6] * lFactor ELSE ccbccaja.impusa[6] * lFactor
                                tt-caja.impcard     = tt-caja.impcard + IF(lMoneda = 'SOLES') THEN ccbccaja.impnac[4] * lFactor ELSE ccbccaja.impusa[4] * lFactor
                                tt-caja.impbd       = tt-caja.impbd + IF(lMoneda = 'SOLES') THEN ccbccaja.impnac[5] * lFactor ELSE ccbccaja.impusa[5] * lFactor
                                tt-caja.impar       = tt-caja.impar + IF(lMoneda = 'SOLES') THEN ccbccaja.impnac[7] * lFactor ELSE ccbccaja.impusa[7] * lFactor
                                tt-caja.impvales    = tt-caja.impvales + IF(lMoneda = 'SOLES') THEN ccbccaja.impnac[10] * lFactor ELSE ccbccaja.impusa[10] * lFactor
                                tt-caja.impchqdia   = tt-caja.impchqdia + IF(lMoneda = 'SOLES') THEN ccbccaja.impnac[2] * lFactor ELSE ccbccaja.impusa[2] * lFactor
                                tt-caja.impchqdif   = tt-caja.impchqdia + IF(lMoneda = 'SOLES') THEN ccbccaja.impnac[3] * lFactor ELSE ccbccaja.impusa[3] * lFactor
                                tt-caja.impefe      = tt-caja.impefe - IF(lMoneda = 'SOLES') THEN ccbccaja.vuenac ELSE ccbccaja.vueusa.
                        
                    END.
                END.
            END.
        END.

    /*END.*/
END.

RELEASE ix-ccbcdocu.

FOR EACH tt-caja :

    ASSIGN  tt-caja.impcaja = tt-caja.impefe + tt-caja.impn-c + tt-caja.impcard + 
            tt-caja.impbd + tt-caja.impar + tt-caja.impvales + tt-caja.impchqdia + tt-caja.impchqdif.

END.

/*

/* MOSTRADOR pagos al contado */
FOR EACH ccbccaja WHERE ccbccaja.codcia = s-codcia AND
                        ccbccaja.coddoc = 'I/C' AND 
                        ccbccaja.tipo = "MOSTRADOR" AND 
                        ccbccaja.fchdoc = lFechaProceso AND
                        ccbccaja.flgest <> 'A'
                        NO-LOCK :

    FIND FIRST gn-div OF ccbccaja NO-LOCK.
    
    lMoneda = IF(ccbccaja.codmon = 1) THEN "SOLES" ELSE "USD".
    FIND FIRST tt-caja WHERE tt-caja.coddiv = ccbccaja.coddiv AND
                                tt-caja.codmon = lMoneda NO-ERROR.
    IF NOT AVAILABLE tt-caja THEN DO:
        CREATE tt-caja.
            ASSIGN  tt-caja.coddiv  = ccbccaja.coddiv
                    tt-caja.codmon  = lMoneda
                    tt-caja.desdiv  = IF(AVAILABLE gn-div) THEN gn-div.desdiv ELSE "".
    END.
    ASSIGN  tt-caja.impefe      = tt-caja.impefe + IF(lMoneda = 'SOLES') THEN ccbccaja.impnac[1] ELSE ccbccaja.impusa[1]
            tt-caja.impn-c      = tt-caja.impn-c + IF(lMoneda = 'SOLES') THEN ccbccaja.impnac[6] ELSE ccbccaja.impusa[6]
            tt-caja.impcard     = tt-caja.impcard + IF(lMoneda = 'SOLES') THEN ccbccaja.impnac[4] ELSE ccbccaja.impusa[4]
            tt-caja.impbd       = tt-caja.impbd + IF(lMoneda = 'SOLES') THEN ccbccaja.impnac[5] ELSE ccbccaja.impusa[5]
            tt-caja.impar       = tt-caja.impar + IF(lMoneda = 'SOLES') THEN ccbccaja.impnac[7] ELSE ccbccaja.impusa[7]
            tt-caja.impvales    = tt-caja.impvales + IF(lMoneda = 'SOLES') THEN ccbccaja.impnac[10] ELSE ccbccaja.impusa[10]            
            tt-caja.impchqdia   = tt-caja.impchqdia + IF(lMoneda = 'SOLES') THEN ccbccaja.impnac[2] ELSE ccbccaja.impusa[2]
            tt-caja.impchqdif   = tt-caja.impchqdia + IF(lMoneda = 'SOLES') THEN ccbccaja.impnac[3] ELSE ccbccaja.impusa[3]
            tt-caja.impefe      = tt-caja.impefe - IF(lMoneda = 'SOLES') THEN ccbccaja.vuenac ELSE ccbccaja.vueusa.
            /*
            tt-caja.impvuelto   = tt-caja.impvuelto + IF(lMoneda = 'SOLES') THEN ccbccaja.vuenac ELSE ccbccaja.vueusa            
            */

    
    ASSIGN tt-caja.impcontado = tt-caja.imptot + tt-caja.impefe + tt-caja.impn-c + tt-caja.impchqdia +
                tt-caja.impchqdif + tt-caja.impcard + tt-caja.impbd + tt-caja.impar.
                    

END.
*/


lFin = NOW.

MESSAGE lIni lFin.

SESSION:SET-WAIT-STATE('').

DEFINE VAR hProc AS HANDLE NO-UNDO.

RUN lib\Tools-to-excel PERSISTENT SET hProc.

def var c-csv-file as char no-undo.
def var c-xls-file as char no-undo. /* will contain the XLS file path created */

c-xls-file = 'c:\ciman\ingresos-caja.xlsx'.

run pi-crea-archivo-csv IN hProc (input  buffer tt-caja:handle,
                        /*input  session:temp-directory + "file"*/ c-xls-file,
                        output c-csv-file) .

run pi-crea-archivo-xls  IN hProc (input  buffer tt-caja:handle,
                        input  c-csv-file,
                        output c-xls-file) .

DELETE PROCEDURE hProc.


END PROCEDURE.

/*
DEFINE TEMP-TABLE tt-caja
    FIELDS coddiv       AS CHAR     FORMAT 'x(6)'   COLUMN-LABEL "DIVISION"
    FIELDS desdiv       AS CHAR     FORMAT 'x(50)'   COLUMN-LABEL "NOMBRE"
    FIELDS codmon       AS CHAR     FORMAT 'x(6)'   COLUMN-LABEL "MONEDA"
    FIELDS impefe       AS DEC      FORMAT 'ZZ,ZZZ,ZZ9.99'   COLUMN-LABEL "EFECITVO"
    FIELDS impn-c       AS DEC      FORMAT 'ZZ,ZZZ,ZZ9.99'   COLUMN-LABEL "N/C"
    FIELDS impchqdia    AS DEC      FORMAT 'ZZ,ZZZ,ZZ9.99'   COLUMN-LABEL "CHEQUE DEL DIA"
    FIELDS impchqdif    AS DEC      FORMAT 'ZZ,ZZZ,ZZ9.99'   COLUMN-LABEL "CHEQUE DIFERIDO"
    FIELDS impcard      AS DEC      FORMAT 'ZZ,ZZZ,ZZ9.99'   COLUMN-LABEL "TARJETA"
    FIELDS impbd        AS DEC      FORMAT 'ZZ,ZZZ,ZZ9.99'   COLUMN-LABEL "BOLETA DEPOSITO"
    FIELDS impar        AS DEC      FORMAT 'ZZ,ZZZ,ZZ9.99'   COLUMN-LABEL "ANTICIPOS"
    FIELDS impvales     AS DEC      FORMAT 'ZZ,ZZZ,ZZ9.99'   COLUMN-LABEL "VALES CONSUMO"
    FIELDS impvuelto    AS DEC      FORMAT 'ZZ,ZZZ,ZZ9.99'   COLUMN-LABEL "VUELTO".
*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

