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
         HEIGHT             = 6.04
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

DEF VAR CMB-Mes AS INT NO-UNDO.
DEF VAR FILL-IN-FchAst AS DATE NO-UNDO.
DEF VAR x-CodOpe AS CHAR NO-UNDO.
DEF VAR x-NroAst AS CHAR NO-UNDO.
DEF VAR s-CodCia AS INT INIT 001 NO-UNDO.
DEF VAR s-Periodo AS INT NO-UNDO.
DEF VAR cb-codcia AS INT INIT 000 NO-UNDO.
DEF VAR s-user-id AS CHAR INIT "AUTOMATICO" NO-UNDO.

DEF TEMP-TABLE t-wmigdi LIKE wmigdi.
ASSIGN
    CMB-Mes = 06
    FILL-IN-FchAst = 06/30/2012
    s-Periodo = YEAR(FILL-IN-FchAst).

IF FILL-IN-fchast = ? THEN RETURN.
/*DETERMINACION DE LA OPERACION Y DEL NUMERO DE ASIENTO CORRESPONDIENTE */
FIND pl-calc WHERE PL-CALC.codpln = 01      /* Empleados */
    AND PL-CALC.codcal = 001                    /* Remuneraciones */
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE pl-calc THEN RETURN.
x-codope = "068".
CASE PL-CALC.CodCal:
    WHEN 009 THEN x-codope = '072'.     /* CTS MENSUAL */
    WHEN 010 THEN x-codope = '078'.     /* PROV. VAC. TRUNCAS */
    WHEN 011 THEN x-codope = '079'.     /* PROV. GRATIF. TRUNCAS */
    OTHERWISE x-codope = '068'.
END CASE.
x-nroast = STRING(CMB-mes,"99") +
    STRING(PL-CALC.codpln,"99") +
    STRING(PL-CALC.codcal,"99").
RUN genera-emp( s-codcia,
                s-periodo,
                CMB-mes,
                x-codope,
                x-nroast,
                PL-CALC.codpln,
                PL-CALC.codcal ).

 RUN Transferir-Asiento.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-genera-emp) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE genera-emp Procedure 
PROCEDURE genera-emp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER p-codcia  AS INTEGER.
DEFINE INPUT PARAMETER p-periodo AS INTEGER.
DEFINE INPUT PARAMETER p-mes     AS INTEGER.
DEFINE INPUT PARAMETER p-codope  AS CHARACTER.
DEFINE INPUT PARAMETER p-nroast  AS CHARACTER.
DEFINE INPUT PARAMETER p-codpln  AS INTEGER.
DEFINE INPUT PARAMETER p-codcal  AS INTEGER.

DEFINE VARIABLE t-tpomov AS LOGICAL.
DEFINE VARIABLE a-tpomov AS LOGICAL.
DEFINE VARIABLE x-ctrcta AS CHARACTER.
DEFINE VARIABLE m-clfaux AS CHARACTER.
DEFINE VARIABLE n-clfaux AS CHARACTER.
DEFINE VARIABLE x-cta    AS CHARACTER.
DEFINE VARIABLE x-imptot AS INTEGER.
DEFINE VARIABLE x-coddiv AS CHAR.
DEFINE VARIABLE x-cco    AS CHAR.

DEFINE VARIABLE cDescAcct AS CHARACTER NO-UNDO.

x-ctrcta = "41111100".
x-imptot = 403.
x-coddiv = "00000".

CASE p-codcal:
    WHEN 009 THEN x-ctrcta = '41511200'.      /* CTS MENSUAL */
    WHEN 010 THEN x-ctrcta = '41151200'.      /* PROV VAC TRUNCAS */
    WHEN 011 THEN x-ctrcta = '41141200'.      /* PROV GRATIF TRUNCAS */
    OTHERWISE x-ctrcta = '41111100'.
END CASE.    

EMPTY TEMP-TABLE t-wmigdi.
FOR EACH PL-BOLE NO-LOCK WHERE PL-BOLE.codpln = p-codpln 
    AND PL-BOLE.codcal = p-codcal 
    AND PL-BOLE.codcta <> "" 
    BREAK BY PL-BOLE.tpobol DESCENDING.
    IF FIRST-OF ( PL-BOLE.tpobol ) THEN DO:
        CASE PL-BOLE.tpobol:
            WHEN "Aportes" OR WHEN "Descuentos" THEN t-tpomov = TRUE.
            WHEN "Remuneraciones" THEN t-tpomov = FALSE.
        END CASE.
        a-tpomov = NOT t-tpomov.
    END.
    MOVIMIENTOS:
    FOR EACH PL-MOV-MES NO-LOCK WHERE PL-MOV-MES.codcia  = p-codcia 
        AND PL-MOV-MES.periodo = p-periodo 
        AND PL-MOV-MES.nromes  = p-mes 
        AND PL-MOV-MES.codpln  = p-codpln 
        AND PL-MOV-MES.codcal  = p-codcal 
        AND PL-MOV-MES.codmov  = PL-BOLE.codmov:
        FIND PL-FLG-MES WHERE PL-FLG-MES.codcia  = p-codcia 
            AND PL-FLG-MES.periodo = p-periodo 
            AND PL-FLG-MES.nromes  = p-mes 
            AND PL-FLG-MES.codpln  = PL-MOV-MES.codpln 
            AND PL-FLG-MES.codper  = PL-MOV-MES.codper NO-LOCK NO-ERROR.
        IF AVAILABLE PL-FLG-MES THEN DO:
            ASSIGN
                n-clfaux = ''
                m-clfaux = 'PE'
                x-cco    = PL-FLG-MES.Ccosto
                x-cta    = PL-BOLE.CodCta.            
            FIND cb-auxi WHERE cb-auxi.codcia = cb-codcia
                AND cb-auxi.clfaux = "CCO"
                AND cb-auxi.codaux = x-cco
                NO-LOCK NO-ERROR.
            IF AVAILABLE cb-auxi AND cb-auxi.libre_c01 <> '' THEN x-cco = cb-auxi.libre_c01.

            IF x-cta BEGINS 'XX' THEN DO:                         
                x-cta = SUBSTR(PL-FLG-MES.Clase,1,2) + SUBSTR(PL-BOLE.Codcta,3). 
            END.                                                                 
            FIND cb-ctas WHERE cb-ctas.codcia = cb-codcia 
                AND cb-ctas.codcta = x-cta NO-LOCK NO-ERROR.
            IF NOT AVAILABLE Cb-Ctas THEN DO:
                cDescAcct = x-cta + ' ' + PL-FLG-MES.CodPer + " CUENTA NO EXISTE !!!".
            END.
            ELSE cDescAcct = cb-ctas.nomcta.
            /*******************************************/
            IF AVAILABLE cb-ctas AND cb-ctas.PidAux THEN DO:
               ASSIGN
                   n-clfaux = "00" + PL-FLG-MES.codper
                   m-clfaux = 'PE'.
               IF x-cta BEGINS '46' THEN DO:
                  FIND PL-AFPS WHERE PL-AFPS.codafp = PL-FLG-MES.codafp NO-LOCK NO-ERROR.
                  IF AVAILABLE PL-AFPS THEN
                     ASSIGN
                         n-clfaux =  STRING(PL-AFPS.codafp,"99")
                         m-clfaux = 'EF'.       /*'@AFP'.*/
               END.
            END.
            IF AVAILABLE cb-ctas AND cb-ctas.pidcco = NO THEN x-cco = ''.
            FIND t-wmigdi WHERE
                t-wmigdi.wactiv = x-coddiv AND
                t-wmigdi.wctcta = x-cta /*PL-BOLE.codcta*/ AND
                t-wmigdi.wtpmov = (IF t-tpomov = YES THEN "A" ELSE "C") AND
                t-wmigdi.wcdau1 = n-clfaux AND
                t-wmigdi.wtpau1 = m-clfaux AND 
                t-wmigdi.wccost = x-cco NO-ERROR.
            IF NOT AVAILABLE t-wmigdi THEN DO:
                CREATE t-wmigdi.
                ASSIGN
                    t-wmigdi.wactiv = x-coddiv
                    t-wmigdi.wctcta = x-cta /*PL-BOLE.codcta*/
                    t-wmigdi.wtpmov = (IF t-tpomov = YES THEN "A" ELSE "C")
                    t-wmigdi.wtpau1 = m-clfaux
                    t-wmigdi.wcdau1 = n-clfaux
                    t-wmigdi.wrefe1 = m-clfaux
                    t-wmigdi.wccost = x-cco
                    t-wmigdi.wdesdt = cDescAcct.
            END.
            ASSIGN
                t-wmigdi.wmonor = 00
                t-wmigdi.wimpor = t-wmigdi.wimpor + PL-MOV-MES.valcal-mes.
            IF PL-BOLE.codcta-cntr = "" THEN NEXT.
            ELSE DO:
                ASSIGN
                    n-clfaux = ''
                    m-clfaux = 'PE'
                    x-cta    = PL-BOLE.CodCta-cntr
                    x-cco    = PL-FLG-MES.Ccosto.
                /* RHC 26.04.2012 */
                IF x-cta BEGINS 'XX' THEN DO:                         
                    x-cta = SUBSTR(PL-FLG-MES.Clase,1,2) + SUBSTR(PL-BOLE.CodCta-cntr,3). 
                END.  
                FIND cb-ctas WHERE cb-ctas.codcia = cb-codcia 
                    AND cb-ctas.codcta = x-cta /*PL-BOLE.codcta-cntr*/ NO-LOCK NO-ERROR.
                IF AVAILABLE cb-ctas AND cb-ctas.PidAux THEN DO:
                   ASSIGN
                       n-clfaux = "00" + PL-FLG-MES.codper
                       m-clfaux = 'PE'.
                   IF x-cta /*PL-BOLE.codcta-cntr*/ BEGINS '46' THEN DO:
                      FIND PL-AFPS WHERE PL-AFPS.codafp = PL-FLG-MES.codafp NO-LOCK NO-ERROR.
                      IF AVAILABLE PL-AFPS THEN
                         ASSIGN
                             n-clfaux = STRING(PL-AFPS.codafp,"99")
                             m-clfaux = 'EF'.       /*'@AFP'*/.
                   END.
                END.
                IF AVAILABLE cb-ctas AND cb-ctas.pidcco = NO THEN x-cco = ''.
                FIND t-wmigdi WHERE
                    t-wmigdi.wactiv = x-coddiv AND                
                    t-wmigdi.wctcta = x-cta /*PL-BOLE.codcta-cntr*/ AND
                    t-wmigdi.wtpmov = (IF a-tpomov = YES THEN "A" ELSE "C") AND
                    t-wmigdi.wcdau1 = n-clfaux AND
                    t-wmigdi.wtpau1 = m-clfaux AND
                    t-wmigdi.wccost = x-cco NO-ERROR.                    
                IF NOT AVAIL t-wmigdi THEN DO:
                    CREATE t-wmigdi.
                    ASSIGN
                        t-wmigdi.wactiv = x-coddiv
                        t-wmigdi.wctcta = x-cta /*PL-BOLE.codcta-CNTR*/
                        t-wmigdi.wtpmov = (IF a-tpomov = YES THEN "A" ELSE "C")
                        t-wmigdi.wtpau1 = m-clfaux
                        t-wmigdi.wcdau1 = n-clfaux
                        t-wmigdi.wrefe1 = m-clfaux
                        t-wmigdi.wccost = x-cco.
                    FIND cb-ctas WHERE cb-ctas.codcia = cb-codcia 
                        AND cb-ctas.codcta = t-wmigdi.wctcta NO-LOCK NO-ERROR.
                    IF AVAILABLE cb-ctas THEN t-wmigdi.wdesdt = cb-ctas.nomcta.
                    ELSE t-wmigdi.wdesdt = "CUENTA NO EXISTE !!!".
                END.
                t-wmigdi.wimpor = t-wmigdi.wimpor + PL-MOV-MES.valcal-mes.
            END.
            
        END.
    END.
END.

ASSIGN
    n-clfaux = ''
    m-clfaux = 'PE'
    a-tpomov = TRUE.
FOR EACH PL-MOV-MES NO-LOCK WHERE PL-MOV-MES.codcia  = p-codcia 
    AND PL-MOV-MES.periodo = p-periodo 
    AND PL-MOV-MES.nromes  = p-mes 
    AND PL-MOV-MES.codpln  = p-codpln 
    AND PL-MOV-MES.codcal  = p-codcal 
    AND PL-MOV-MES.codmov  = x-imptot:
    FIND PL-FLG-MES WHERE PL-FLG-MES.codcia  = p-codcia 
        AND PL-FLG-MES.periodo = p-periodo 
        AND PL-FLG-MES.nromes  = p-mes 
        AND PL-FLG-MES.codpln  = PL-MOV-MES.codpln 
        AND PL-FLG-MES.codper  = PL-MOV-MES.codper NO-LOCK NO-ERROR.
    IF AVAILABLE PL-FLG-MES THEN DO:
        FIND t-wmigdi WHERE
            t-wmigdi.wactiv = x-coddiv AND    
            t-wmigdi.wctcta = x-ctrcta AND
            t-wmigdi.wtpmov = (IF a-tpomov = YES THEN "A" ELSE "C") AND
            t-wmigdi.wcdau1 = n-clfaux AND
            t-wmigdi.wtpau1 = m-clfaux NO-ERROR.                    
        IF NOT AVAIL t-wmigdi THEN DO:
            CREATE t-wmigdi.
            ASSIGN
                t-wmigdi.wactiv = x-coddiv
                t-wmigdi.wctcta = x-ctrcta
                t-wmigdi.wtpmov = (IF a-tpomov = YES THEN "A" ELSE "C")
                t-wmigdi.wtpau1 = m-clfaux
                t-wmigdi.wcdau1 = n-clfaux
                t-wmigdi.wrefe1 = m-clfaux.
            FIND cb-ctas WHERE cb-ctas.codcia = cb-codcia 
                AND cb-ctas.codcta = t-wmigdi.wctcta NO-LOCK NO-ERROR.
            IF AVAILABLE cb-ctas THEN t-wmigdi.wdesdt = cb-ctas.nomcta.
            ELSE t-wmigdi.wdesdt = "CUENTA NO EXISTE !!!".
        END.
        t-wmigdi.wimpor = t-wmigdi.wimpor + PL-MOV-MES.valcal-mes.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Transferir-Asiento) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Transferir-Asiento Procedure 
PROCEDURE Transferir-Asiento :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/* BLOQUEAMOS Sí o Sí el correlativo */
DEF VAR x-Secuencia AS INT INIT 1 NO-UNDO.

REPEAT:
  FIND wmigcorr WHERE wmigcorr.Proceso = "PL"
      AND wmigcorr.Periodo = s-Periodo
      AND wmigcorr.Mes = CMB-Mes
      EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
  IF NOT AVAILABLE wmigcorr THEN DO:
      IF NOT LOCKED wmigcorr THEN DO:
          /* CREAMOS EL CONTROL */
          CREATE wmigcorr.
          ASSIGN
              wmigcorr.Correlativo = 1
              wmigcorr.Mes = CMB-Mes
              wmigcorr.Periodo = s-Periodo
              wmigcorr.Proceso = "PL".
          LEAVE.
      END.
      ELSE UNDO, RETRY.
  END.
  LEAVE.
END.
FOR EACH T-WMIGDI NO-LOCK:
    CREATE WMIGDI.
    BUFFER-COPY T-WMIGDI
        TO WMIGDI
        ASSIGN
        WMIGDI.FlagFecha = DATETIME(TODAY, MTIME)
        WMIGDI.FlagTipo = "I"
        WMIGDI.FlagUsuario = s-user-id
        WMIGDI.wcorre = STRING(wmigcorr.Periodo, '9999') + STRING(wmigcorr.Mes, '99') +
                        STRING(wmigcorr.Correlativo, '9999')
        WMIGDI.wsecue = x-Secuencia
        WMIGDI.wtpasi = "DI"
        WMIGDI.wcdasi = "1"
        WMIGDI.wfeasi = YEAR(TODAY) * 10000 + MONTH(TODAY) * 100 + DAY(TODAY)
        WMIGDI.wdescb = "PLANILLA DE REMUNERACIONES"
        WMIGDI.wfemov = YEAR(TODAY) * 10000 + MONTH(TODAY) * 100 + DAY(TODAY)
        WMIGDI.wrefe5 = STRING(wmigcorr.Periodo, '9999') + STRING(wmigcorr.Mes, '99') + 
                        STRING(wmigcorr.Correlativo, '9999').
    x-Secuencia = x-Secuencia + 1.
END.
ASSIGN
    wmigcorr.Correlativo = wmigcorr.Correlativo + 1.
IF AVAILABLE(wmigcorr) THEN RELEASE wmigcorr.
IF AVAILABLE(WMIGDI)   THEN RELEASE WMIGDI.
EMPTY TEMP-TABLE T-WMIGDI.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

