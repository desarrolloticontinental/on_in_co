&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*--------------------------------------------------------------------------
    File        : r-impgui.p
    Purpose     : Impresion de Guias

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER X-ROWID AS ROWID.

DEF SHARED VAR s-codcia  AS INT.
DEF SHARED VAR CL-codcia  AS INT.
DEF SHARED VAR PV-codcia  AS INT.
DEF SHARED VAR s-coddiv  LIKE gn-divi.coddiv.
DEF SHARED VAR s-codter  LIKE ccbcterm.codter.
DEF SHARED VAR s-user-id LIKE ccbcterm.codter.
DEF SHARED VAR X-NUMORD  AS CHAR.
DEF SHARED VAR X-obser  AS CHAR format "x(80)".

DEF VAR C-DirCli AS CHAR FORMAT "X(70)".
DEF VAR X-EnLetras AS CHAR FORMAT "x(100)" NO-UNDO.
DEF VAR C-NomVen AS CHAR FORMAT "X(21)".
DEF VAR C-NomCon AS CHAR FORMAT "X(30)".
DEF VAR C-RucTra AS CHAR FORMAT "X(30)".
DEF VAR C-NomTra AS CHAR FORMAT "X(30)".
DEF VAR C-DirTra AS CHAR FORMAT "X(30)".
DEF VAR C-Marca  AS CHAR FORMAT "X(30)".
DEF VAR C-Placa  AS CHAR FORMAT "X(10)".
DEF VAR I-NroItm AS INTEGER.
DEF VAR S-TOTPES AS DECIMAL.
DEF VAR X-senal  AS CHAR.
DEF VAR X-ZONA   AS CHAR.
DEF VAR X-DEPART LIKE TabDepto.NomDepto.
DEF VAR X-LugPar LIKE Almacen.DirAlm.
DEF VAR X-Nombre LIKE gn-prov.NomPro.
DEF VAR X-ruc    LIKE gn-prov.Ruc.
DEF VAR X-TRANS  LIKE FACCPEDI.Libre_c01.
DEF VAR X-DIREC  LIKE FACCPEDI.Libre_c02.
DEF VAR X-LUGAR  LIKE FACCPEDI.Libre_c03.
DEF VAR X-CONTC  LIKE FACCPEDI.Libre_c04.
DEF VAR X-HORA   LIKE FACCPEDI.Libre_c05.
/* RHC 20/07/2015 DATOS DEL TRANSPORTISTA */
DEF VAR f-CodAge AS CHAR NO-UNDO.
DEF VAR f-NomTra AS CHAR NO-UNDO.
DEF VAR f-RucAge AS CHAR NO-UNDO.
DEF VAR f-Marca  AS CHAR NO-UNDO.
DEF VAR f-NroLicencia AS CHAR NO-UNDO.
DEF VAR f-Placa  AS CHAR NO-UNDO.
/* ************************************** */

DEFINE VAR lDirFiscal AS CHAR.

DEFINE SHARED VARIABLE X-CTRANS  AS CHAR. 
DEFINE SHARED VARIABLE X-RTRANS  AS CHAR. 
DEFINE SHARED VARIABLE X-DTRANS  AS CHAR. 

DEFINE VAR dPesTot AS DEC NO-UNDO.

FIND ccbcdocu WHERE ROWID(ccbcdocu) = X-ROWID NO-LOCK NO-ERROR.
IF NOT AVAILABLE ccbcdocu THEN RETURN.

FIND gn-clie WHERE gn-clie.codcia = CL-CODCIA 
    AND gn-clie.codcli = ccbcdocu.codcli 
    NO-LOCK.
    
ASSIGN
    X-senal  = "X"
    C-NomVen = CcbCDocu.CodVen
    C-NomCon = CcbCDocu.FmaPgo.
IF CcbCDocu.CodDpto <> "" AND CcbCDocu.CodProv <> "" AND CcbCDocu.CodDist <> "" THEN DO:
  IF CcbCDocu.CodDpto = "15" AND CcbCDocu.CodProv = "01" THEN do:
     FIND TabDistr WHERE TabDistr.CodDepto = "15" 
                    AND  TabDistr.CodProvi = "01" 
                    AND  TabDistr.CodDistr = CcbCDocu.CodDist 
                   NO-LOCK NO-ERROR.
     IF AVAILABLE TabDistr THEN X-ZONA = TabDistr.NomDistr.
  END.    
  ELSE DO:     
     FIND TabProvi WHERE TabProvi.CodDepto = CcbCDocu.CodDpto 
                    AND  TabProvi.CodProvi = CcbCDocu.CodProv 
                   NO-LOCK NO-ERROR.
     IF AVAILABLE TabProvi THEN X-ZONA = TabProvi.NomProvi.
  END.        
END.

FIND gn-ven WHERE gn-ven.CodCia = CcbCDocu.CodCia 
             AND  gn-ven.CodVen = CcbCDocu.CodVen 
            NO-LOCK NO-ERROR.
IF AVAILABLE gn-ven THEN C-NomVen = gn-ven.NomVen.

FIND gn-ConVt WHERE gn-ConVt.Codig = CcbCDocu.FmaPgo NO-LOCK NO-ERROR.
IF AVAILABLE gn-ConVt THEN C-NomCon = C-NomCon + " - " + gn-ConVt.Nombr.

FIND GN-PROV WHERE gn-prov.codcia = PV-CODCIA AND
                   GN-PROV.CODPRO = CcbCDocu.CodAge NO-LOCK NO-ERROR.
IF AVAILABLE GN-PROV THEN DO:
    c-NomTra = GN-PROV.NOMPRO.
    c-DirTra = gn-prov.DirPro.
    c-RucTra = gn-prov.Ruc.
    c-Marca  = ccbcdocu.codcta.
    c-Placa  = ccbcdocu.nrocard.
END.

/* puntero en posicion */
FIND Ccbadocu OF Ccbcdocu NO-LOCK NO-ERROR.
IF AVAILABLE Ccbadocu THEN DO:
    ASSIGN
        x-Trans = CcbADocu.Libre_C[9]
        x-Direc = CcbADocu.Libre_C[12]
        x-Lugar = CcbADocu.Libre_C[13]
        x-Contc = CcbADocu.Libre_C[14]
        x-Hora  = CcbADocu.Libre_C[15].
    ASSIGN
        f-CodAge = Ccbadocu.Libre_C[3]
        f-NomTra = Ccbadocu.Libre_C[4]
        f-RucAge = Ccbadocu.Libre_C[5]
        f-Marca  = Ccbadocu.Libre_C[2]
        f-NroLicencia = Ccbadocu.Libre_C[6]
        f-Placa  = Ccbadocu.Libre_C[1].
END.
FIND gn-prov WHERE gn-prov.CodCia = PV-CODCIA 
    AND gn-prov.CodPro = X-TRANS 
    NO-LOCK NO-ERROR.
IF AVAILABLE gn-prov THEN                                     
     ASSIGN 
           X-Nombre = gn-prov.NomPro
           X-Ruc    = gn-prov.Ruc.
FIND gn-divi WHERE gn-divi.codcia = s-codcia
    AND gn-divi.coddiv = Ccbcdocu.coddiv
    NO-LOCK NO-ERROR.
IF AVAILABLE gn-divi THEN x-LugPar = TRIM(GN-DIVI.DirDiv) + " " + gn-divi.faxdiv.
FIND TabDepto WHERE TabDepto.CodDepto = CcbCDocu.CodDpto NO-LOCK NO-ERROR.
IF AVAILABLE TabDepto THEN X-DEPART = TabDepto.NomDepto.

/* ********************* Direccion del cliente ************************************* */

/* 
    Ic - 07Nov2017, correo de C.Camus, debe imprimir en la G/R el lugar de entrega
*/

c-DirCli = TRIM(Ccbcdocu.dircli).
FIND TabDepto WHERE TabDepto.CodDepto = gn-clie.CodDept NO-LOCK NO-ERROR.
IF AVAILABLE TabDepto THEN c-DirCli = c-DirCli + ' - ' + TRIM(TabDepto.NomDepto).
FIND TabProvi WHERE TabProvi.CodDepto = gn-clie.CodDept 
    AND TabProvi.CodProvi = gn-clie.CodProv NO-LOCK NO-ERROR.
IF AVAILABLE TabProvi THEN c-DirCli = c-DirCli + ' - ' + TRIM(TabProvi.NomProvi).
FIND TabDistr WHERE TabDistr.CodDepto = gn-clie.CodDept 
    AND TabDistr.CodProvi = gn-clie.CodProv
    AND TabDistr.CodDistr = gn-clie.CodDist NO-LOCK NO-ERROR.
IF AVAILABLE TabDistr THEN  c-DirCli = c-DirCli + ' - ' + TRIM(TabDistr.NomDistr).


/* Ic - 08Nov2017, camus dejar sin efecto mientras se cordina. */
/*
c-DirCli = TRIM(Ccbcdocu.lugent).
IF c-DirCli = ? OR c-DirCli = "" THEN DO:
    c-DirCli = TRIM(Ccbcdocu.dircli).
    FIND TabDepto WHERE TabDepto.CodDepto = gn-clie.CodDept NO-LOCK NO-ERROR.
    IF AVAILABLE TabDepto THEN c-DirCli = c-DirCli + ' - ' + TRIM(TabDepto.NomDepto).
    FIND TabProvi WHERE TabProvi.CodDepto = gn-clie.CodDept 
        AND TabProvi.CodProvi = gn-clie.CodProv NO-LOCK NO-ERROR.
    IF AVAILABLE TabProvi THEN c-DirCli = c-DirCli + ' - ' + TRIM(TabProvi.NomProvi).
    FIND TabDistr WHERE TabDistr.CodDepto = gn-clie.CodDept 
        AND TabDistr.CodProvi = gn-clie.CodProv
        AND TabDistr.CodDistr = gn-clie.CodDist NO-LOCK NO-ERROR.
    IF AVAILABLE TabDistr THEN  c-DirCli = c-DirCli + ' - ' + TRIM(TabDistr.NomDistr).
END.
*/
/* ********************************************************************************* */

/************************  PUNTEROS EN POSICION  *******************************/
RUN bin/_numero(ccbcdocu.imptot, 2, 1, OUTPUT X-EnLetras).
X-EnLetras = X-EnLetras + (IF ccbcdocu.codmon = 1 THEN " NUEVOS SOLES" ELSE " DOLARES AMERICANOS").

/************************  DEFINICION DE FRAMES  *******************************/

lDirFiscal = 'DIR.FISCAL : CAL.RENE DESCARTES NRO.114 URB. SANTA RAQUEL II ETAPA, LIMA-LIMA-ATE'.

DEFINE FRAME F-HdrGui
    HEADER
    /*SKIP(5)*/
    SKIP(2)
    lDirFiscal      AT 12 FORMAT 'x(100)' SKIP
    SKIP(2)
    gn-divi.dirdiv  AT 12 FORMAT 'x(80)' WHEN gn-divi.coddiv = '00501' SKIP
    x-LugPar        AT 12 FORMAT 'x(100)' SKIP
    c-dircli        AT 12 FORMAT "X(100)" SKIP
    ccbcdocu.nomcli AT 12 FORMAT "X(70)" SKIP
    ccbcdocu.RucCli AT 12 FORMAT "X(11)" 
    CcbCDocu.CodCli AT 12 FORMAT "X(11)" 
    f-NomTra        AT 90 FORMAT "X(40)" SKIP
    ccbcdocu.nrodoc AT 35 FORMAT "XXX-XXXXXX" 
    ccbcdocu.libre_c01 AT 50 FORMAT "X(3)" ccbcdocu.libre_c02 FORMAT "XXX-XXXXXX"
    f-RucAge        AT 87 FORMAT "X(11)" 
    f-Marca         AT 120 SKIP
    f-NroLicencia   AT 90 FORMAT 'x(15)'
    f-Placa         AT 115 SKIP(3)
    CcbCDocu.CodVen AT 85 FORMAT "X(3)"
    CcbCDocu.NroRef AT 99 FORMAT "XXX-XXXXXXXXX" 
    ccbcdocu.fchdoc AT 119
    SKIP(2) 
    WITH PAGE-TOP NO-LABELS NO-BOX STREAM-IO WIDTH 200.
/* DEFINE FRAME F-HdrGui                                                             */
/*     HEADER                                                                        */
/*     SKIP(5)                                                                       */
/*     gn-divi.dirdiv  AT 12 FORMAT 'x(80)' WHEN gn-divi.coddiv = '00501' SKIP       */
/*     x-LugPar        AT 12 FORMAT 'x(100)' SKIP                                    */
/*     c-dircli        AT 12 FORMAT "X(100)" SKIP                                    */
/*     ccbcdocu.nomcli AT 12 FORMAT "X(70)" SKIP                                     */
/*     ccbcdocu.RucCli AT 12 FORMAT "X(11)"                                          */
/*     CcbCDocu.CodCli AT 12 FORMAT "X(11)"                                          */
/*     C-NomTra        AT 90 FORMAT "X(40)" SKIP                                     */
/*     ccbcdocu.nrodoc AT 35 FORMAT "XXX-XXXXXX"                                     */
/*     ccbcdocu.libre_c01 AT 50 FORMAT "X(3)" ccbcdocu.libre_c02 FORMAT "XXX-XXXXXX" */
/*     C-RucTra        AT 87 FORMAT "X(11)"                                          */
/*     c-Marca         AT 120 SKIP                                                   */
/*     C-Placa         AT 115 SKIP(3)                                                */
/*     CcbCDocu.CodVen AT 85 FORMAT "X(3)"                                           */
/*     CcbCDocu.NroRef AT 99 FORMAT "XXX-XXXXXX"                                     */
/*     ccbcdocu.fchdoc AT 119                                                        */
/*     SKIP(2)                                                                       */
/*     WITH PAGE-TOP NO-LABELS NO-BOX STREAM-IO WIDTH 200.                           */

DEFINE FRAME F-DetaGui
    ccbcdocu.codalm  AT 02 FORMAT "99"
    I-NroItm         AT 04 FORMAT "Z9"
    ccbddocu.codmat  AT 11 FORMAT "999999"
    almmmatg.desmat  AT 27 FORMAT "X(40)"
    almmmatg.desmar  AT 91 FORMAT "X(10)"
    ccbddocu.candes  AT 108 FORMAT ">>>,>>9.99"
    ccbddocu.UndVta  AT 119 FORMAT "X(8)"
    WITH NO-LABELS NO-BOX NO-UNDERLINE STREAM-IO WIDTH 200.

DEFINE FRAME F-FtrGui
    HEADER
    "LA MERCADERIA VIAJA POR CUENTA Y RIESGO DEL CLIENTE, EL DESPACHO DE MERCADERIA ABARCA HASTA LA AGENCIA DE TRANSPORTE" skip(2)
    'Contacto : ' X-CONTC FORMAT 'X(25)' 'Hora Aten :' X-HORA FORMAT 'X(10)'SKIP
    'PRIMER TRAMO  : '     SKIP
    'Transport: ' X-Nombre FORMAT 'X(50)' SKIP
    'RUC      : ' X-ruc    FORMAT 'X(11)' dPesTot AT 58 SKIP
    'Direcci�n: ' X-DIREC  FORMAT 'X(50)' SKIP
    'SEGUNDO TRAMO  : '    SKIP
    'Destino  : ' X-LUGAR  FORMAT 'X(50)' SKIP
    'Observ   : ' CcbCDocu.Glosa VIEW-AS TEXT FORMAT "X(60)" 
    ' ' AT 75 SKIP
/*     'Peso Aproximado(Kg.): ' AT 75 dPesTot AT 100 SKIP */
    WITH PAGE-BOTTOM NO-LABELS NO-UNDERLINE NO-BOX STREAM-IO WIDTH 200.

DEFINE FRAME F-FtrGui2
    HEADER
    "LA MERCADERIA VIAJA POR CUENTA Y RIESGO DEL CLIENTE, EL DESPACHO DE MERCADERIA ABARCA HASTA LA AGENCIA DE TRANSPORTE" skip(6)
    WITH PAGE-BOTTOM NO-LABELS NO-UNDERLINE NO-BOX STREAM-IO WIDTH 200.

DEFINE FRAME F-FtrGui3
    HEADER
    "LA MERCADERIA VIAJA POR CUENTA Y RIESGO DEL CLIENTE, EL DESPACHO DE MERCADERIA ABARCA HASTA LA AGENCIA DE TRANSPORTE" skip(1)
    dPesTot AT 58 SKIP
    'PRIMER TRAMO  : '     SKIP
    'Transport: ' X-Nombre FORMAT 'X(50)' SKIP
    'RUC      : ' X-ruc    FORMAT 'X(11)' SKIP
    'Direcci�n: ' X-DIREC  FORMAT 'X(50)' SKIP
    WITH PAGE-BOTTOM NO-LABELS NO-UNDERLINE NO-BOX STREAM-IO WIDTH 200.

DEFINE FRAME F-FtrGui4
    HEADER
    "LA MERCADERIA VIAJA POR CUENTA Y RIESGO DEL CLIENTE, EL DESPACHO DE MERCADERIA ABARCA HASTA LA AGENCIA DE TRANSPORTE" skip(1)  
    dPesTot AT 58 SKIP
    'SEGUNDO TRAMO  : '    SKIP
    'Destino  : ' X-LUGAR  FORMAT 'X(50)' SKIP
    'Observ   : ' CcbCDocu.Glosa VIEW-AS TEXT FORMAT "X(60)" SKIP
    'Contacto : ' X-CONTC FORMAT 'X(25)' 'Hora Aten :' X-HORA FORMAT 'X(10)'SKIP
    WITH PAGE-BOTTOM NO-LABELS NO-UNDERLINE NO-BOX STREAM-IO WIDTH 200.

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
         HEIGHT             = 5.96
         WIDTH              = 53.29.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Procedure 
/* ************************* Included-Libraries *********************** */

{src/bin/_prns.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
DEFINE VAR pStatus AS LOG NO-UNDO.
SYSTEM-DIALOG PRINTER-SETUP UPDATE pStatus.
IF pStatus = NO THEN RETURN.

OUTPUT TO PRINTER PAGE-SIZE 43.
PUT CONTROL {&PRN0} + {&PRN5A} + CHR(48) + {&PRN3}.     

/* Ic - 28Ene2016, para valesUtilex*/
/* Busco la cotizacion */
DEFINE VAR cEsValeUtilex AS LOG INIT NO.
DEFINE VAR cNroVales AS CHAR.

DEFINE BUFFER b-faccpedi FOR faccpedi.
DEFINE BUFFER c-faccpedi FOR faccpedi.

FIND FIRST b-faccpedi WHERE b-faccpedi.codcia = s-codcia AND
                        b-faccpedi.coddoc = 'PED' AND
                        b-faccpedi.nroped = ccbcdocu.nroped NO-LOCK NO-ERROR.
IF AVAILABLE b-faccpedi THEN DO:
    FIND FIRST c-faccpedi WHERE c-faccpedi.codcia = s-codcia AND
                            c-faccpedi.coddoc = 'COT' AND
                            c-faccpedi.nroped = b-faccpedi.nroref NO-LOCK NO-ERROR.
    IF AVAILABLE c-faccpedi THEN DO:
        IF c-faccpedi.tpoped = 'VU' THEN cEsValeUtilex = YES.
    END.
END.
RELEASE b-faccpedi.
RELEASE c-faccpedi.


/*Peso Totales*/
ASSIGN
    dPesTot = 0
    I-NroItm = 0.
FOR EACH Ccbddocu  OF Ccbcdocu NO-LOCK, 
    FIRST almmmatg OF ccbddocu NO-LOCK
    BREAK BY ccbddocu.nrodoc BY ccbddocu.nroitm:
    VIEW FRAME F-HdrGui.
    /*dPesTot = dPesTot + (CcbDDocu.CanDes * CcbDDocu.Factor * Almmmatg.PesMat).      */
    dPesTot = dPesTot + CcbDDocu.pesmat.
    IF X-TRANS <> '' AND  X-LUGAR <> '' THEN VIEW FRAME F-FtrGui.
    IF X-TRANS <> '' AND X-LUGAR = '' THEN VIEW FRAME F-FtrGui3.
    IF X-TRANS = '' AND X-LUGAR <> '' THEN VIEW FRAME F-FtrGui4.
    IF X-TRANS = '' AND X-LUGAR = '' THEN VIEW FRAME F-FtrGui2.
    I-NroItm = I-NroItm + 1.     

    /* Ic - 28Ene2016 ValesUtiles */
    cNroVales = ''.
    IF cEsValeUtilex = YES THEN DO:
        cNroVales = "  (del " + STRING(ccbddocu.impdcto_adelanto[1],">>>>>>>>>9") + 
                    "  al " + STRING(ccbddocu.impdcto_adelanto[2],">>>>>>>>>9") + ")".
    END.

    DISPLAY 
        ccbcdocu.codalm
        I-NroItm 
        ccbddocu.codmat 
        almmmatg.desmat + ' ' + cNroVales @ almmmatg.desmat
        almmmatg.desmar
        ccbddocu.candes 
        ccbddocu.undvta 
        WITH FRAME F-DetaGui.
    IF LAST-OF(ccbddocu.nrodoc) THEN PAGE.
END.
OUTPUT CLOSE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


