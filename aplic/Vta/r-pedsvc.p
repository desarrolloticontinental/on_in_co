&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*--------------------------------------------------------------------------
    File        : r-ImpPed.p
    Purpose     : Impresion de Pedidos y Cotizaciones
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

DEF SHARED VAR S-USER-ID AS CHAR. 
DEF SHARED VAR s-codcia  AS INT.
DEF SHARED VAR cl-CODCIA AS INTEGER.
DEF SHARED VAR s-codalm  AS CHAR.
DEF SHARED VAR s-coddiv  LIKE gn-divi.coddiv.
DEF SHARED VAR s-codter  LIKE ccbcterm.codter.
DEF SHARED VAR S-NomCia  AS CHAR.
DEF        VAR C-NomVen  AS CHAR FORMAT "X(30)".
DEF        VAR C-Moneda  AS CHAR FORMAT "X(7)".
DEF        VAR C-NomCon  AS CHAR FORMAT "X(30)".
DEF        VAR C-TitDoc  AS CHAR FORMAT "X(50)".
DEF        VAR XD        AS CHAR FORMAT "X(2)".
DEF        VAR I-NroItm  AS INTEGER.
DEF        VAR F-PreNet  AS DECIMAL.
DEF        VAR W-DIRALM  AS CHAR FORMAT "X(50)".
DEF        VAR W-TLFALM  AS CHAR FORMAT "X(13)".

DEFINE VARIABLE x-dscto AS DECIMAL.

DEFINE VARIABLE X-IMPIGV AS CHARACTER FORMAT "X(30)".
DEF VAR X-EnLetras AS CHAR FORMAT "x(100)" NO-UNDO.

FIND FacCPedi WHERE ROWID(FacCPedi) = X-ROWID NO-LOCK NO-ERROR.

IF NOT AVAILABLE FacCPedi THEN RETURN.

     C-TitDoc = "    PEDIDO :". 

IF FacCpedi.Codmon = 2 THEN C-Moneda = "DOLARES US$.".
ELSE C-Moneda = "SOLES   S/. ".

C-NomVen = FacCPedi.CodVen.
C-NomCon = FacCPedi.FmaPgo.
XD       = STRING (FacCpedi.Fchven - FacCpedi.Fchped,"99").
X-IMPIGV = IF FacCpedi.FlgIgv THEN "LOS PRECIOS INCLUYEN EL I.G.V."
           ELSE "LOS PRECIOS NO INCLUYEN EL IGV.".


FIND gn-clie WHERE 
     gn-clie.codcia = cl-codcia AND  
     gn-clie.codcli = FacCPedi.codcli NO-LOCK NO-ERROR.
     
FIND gn-ven WHERE 
     gn-ven.CodCia = FacCPedi.CodCia AND  
     gn-ven.CodVen = FacCPedi.CodVen 
     NO-LOCK NO-ERROR.
     
IF AVAILABLE gn-ven THEN C-NomVen = C-NomVen + " - " + gn-ven.NomVen.

FIND gn-ConVt WHERE gn-ConVt.Codig = FacCPedi.FmaPgo NO-LOCK NO-ERROR.
IF AVAILABLE gn-ConVt THEN C-NomCon = gn-ConVt.Nombr.

FIND Almacen WHERE 
     Almacen.CodCia = S-CODCIA AND  
     Almacen.CodAlm = S-CODALM 
     NO-LOCK NO-ERROR.
IF AVAILABLE Almacen THEN  W-DIRALM = Almacen.DirAlm. 
W-TLFALM = Almacen.TelAlm. 

/************************  PUNTEROS EN POSICION  *******************************/
RUN bin/_numero(FacCPedi.imptot, 2, 1, OUTPUT X-EnLetras).
X-EnLetras = "SON : " + X-EnLetras + (IF FacCPedi.codmon = 1 THEN " NUEVOS SOLES" ELSE " DOLARES AMERICANOS").

/************************  DEFINICION DE FRAMES  *******************************/

DEFINE FRAME F-DetaPed
    I-NroItm FORMAT ">>>9"
    FacDPedi.codmat FORMAT "X(6)"
    FacDPedi.CanPed FORMAT ">>>,>>9.99"
    FacDPedi.undvta FORMAT "X(4)"
    almmmatg.desmat FORMAT "X(45)"
    FacDPedi.preuni FORMAT "->>,>>9.9999"
    FacDPedi.PorDto FORMAT ">>9.99%" 
    FacDPedi.implin FORMAT "->,>>>,>>9.99" SKIP
    WITH NO-LABELS NO-BOX NO-UNDERLINE STREAM-IO WIDTH 160.

DEFINE FRAME F-FtrPed
    HEADER
    X-EnLetras AT 10 SKIP
    "NETO A PAGAR : " TO 102 SUBSTRING(C-MONEDA,9,4) FacCPedi.imptot FORMAT ">>,>>>,>>9.99" SKIP(2)
    "OBSERVACIONES : " SKIP 
    FacCPedi.Glosa VIEW-AS TEXT FORMAT "X(80)" SKIP
    "                                                  " SKIP
    "                                                  " SKIP
    "                                                  " SKIP
    "                                                  " SKIP
    "                                                  " SKIP
    "                                                      -------------------     -------------------    -------------------" SKIP
    "                                                          Operador(a)         VoBo Jefe de Ventas       VoBo Cta.Cte.   " SKIP
    "HORA : " TO 1 STRING(TIME,"HH:MM:SS")  S-USER-ID TO 67 SKIP 
    WITH PAGE-BOTTOM NO-LABELS NO-BOX STREAM-IO WIDTH 160.

DEFINE FRAME F-FtrCot
    HEADER
    X-EnLetras AT 10 SKIP
    "NETO A PAGAR : " TO 102 SUBSTRING(C-MONEDA,9,4) FacCPedi.imptot FORMAT ">>,>>>,>>9.99" SKIP(2)
    "OBSERVACIONES : " SKIP 
    FacCPedi.Glosa VIEW-AS TEXT FORMAT "X(80)" SKIP
    "Mercaderia viaja por cuenta y riesgo del cliente. " SKIP
    "--------------------------------------------------" SKIP
    FacCPedi.Observa VIEW-AS TEXT FORMAT "X(150)" SKIP 
    /*EDITOR  INNER-CHARS 40 INNER-LINES 3 */
    "--------------------------------------------------                            ------------------- " SKIP
    "                                                                              VoBo Jefe de Ventas " SKIP
    "HORA : " AT 1 STRING(TIME,"HH:MM:SS") "  " S-USER-ID SKIP  
    WITH PAGE-BOTTOM NO-LABELS NO-BOX STREAM-IO WIDTH 160.

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
         HEIGHT             = 2
         WIDTH              = 40.
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
/* Definimos impresoras */

DEF VAR rpta AS LOG NO-UNDO.
SYSTEM-DIALOG PRINTER-SETUP UPDATE rpta.
IF rpta = NO THEN RETURN.

DEFINE VARIABLE X-ORDCOM AS CHARACTER FORMAT "X(18)".

IF FacCPedi.coddoc = "PED" THEN 
    X-ORDCOM = "Orden de Compra : ".
ELSE 
    X-ORDCOM = "Solicitud Cotiz.: ".

DEFINE FRAME F-HdrPed
    HEADER 
    {&PRN2} + {&PRN7A} + {&PRN6A} + S-NOMCIA + {&PRN6B} + {&PRN7B} + {&PRN3} FORMAT "X(45)" 
    "( " + FacCPedi.CodDiv + ")" AT 1 W-DIRALM AT 10 
    {&PRN7A} + {&PRN6A} + C-TitDoc + {&PRN6B} + {&PRN7B} + {&PRN3} AT 82 FORMAT "X(22)" 
    {&PRN7A} + {&PRN6A} + FacCPedi.NroPed + {&PRN6B} + {&PRN7B} + {&PRN3} AT 104 FORMAT "XXXXXX-XXXXXXXXXXXX" SKIP
    "TELEFAX. " AT 2 W-TLFALM AT 11  "Se�or(es) : " TO 40 FaccPedi.Codcli FORMAT "X(8)" TO 50 FaccPedi.nomcli FORMAT "x(50)" SKIP 
    "Direccion : " TO 40 FacCPedi.DirCli FORMAT "x(40)" "Emision         : " TO 100 FacCPedi.FchPed SKIP
    "R.U.C.    : " TO 40 gn-clie.Ruc    "Vencimiento     : " TO 100 FacCPedi.fchven FORMAT "99/99/9999" SKIP
    "<< OFICINA >> " TO 6
    "Vendedor  : " TO 40 C-NomVen       X-ORDCOM TO 100 FacCPedi.ordcmp SKIP
    "Cond.Venta: " TO 40 C-NomCon       "Moneda          : " TO 100 C-Moneda        SKIP
    "-------------------------------------------------------------------------------------------------------------" SKIP
    "ITEM CODIGO   CANTIDAD UND.             D E S C R I P C I O N               PRECI_VTA   DSCTO    TOTAL NETO  " SKIP
    "-------------------------------------------------------------------------------------------------------------" SKIP
   /*>>>9 123456 >>>,>>9.99 1234 123456789012345678901234567890123456789012345 ->>,>>9.9999 >>9.99% ->,>>>,>>9.99*/
    WITH PAGE-TOP NO-LABELS NO-BOX STREAM-IO WIDTH 160.
                         
    OUTPUT TO PRINTER PAGE-SIZE 60.
    PUT CONTROL {&PRN0} + {&PRN5A} + CHR(66) + {&PRN3}.     
              
    I-NroItm = 0.
    FOR EACH FacDPedi NO-LOCK WHERE 
             FacDPedi.CodCia = FacCPedi.CodCia AND  
             FacDPedi.CodDoc = FacCPedi.CodDoc AND  
             FacDPedi.NroPed = FacCPedi.NroPed , 
             FIRST Almmserv OF FacDPedi NO-LOCK 
             BREAK BY FacDPedi.NroPed:
        I-NroItm = I-NroItm + 1.
        F-PreNet = FacDPedi.preuni * ( 1 - FacDPedi.PorDto / 100 ).
        VIEW FRAME F-HdrPed.
        VIEW FRAME F-FtrPed.
        x-dscto = 0.
        DISPLAY I-NroItm
                FacDPedi.codmat
                FacDPedi.CanPed
                FacDPedi.undvta
                Almmserv.desmat
                FacDPedi.preuni
                FacDPedi.PorDto  WHEN (FacDPedi.PorDto > 0)
                FacDPedi.implin 
                WITH FRAME F-DetaPed.
        IF LAST-OF(FacDPedi.NroPed) THEN DO:
             PAGE.
        END.
    END.

OUTPUT CLOSE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

