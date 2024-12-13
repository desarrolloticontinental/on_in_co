&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*--------------------------------------------------------------------------
    File        : r-bole01.p
    Purpose     : Impresion de Fact/Boletas 
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
DEFINE INPUT PARAMETER pTipo   AS CHAR.
/* pTipo:
    "O": ORIGINAL 
    "C": COPIA 
*/    

DEF SHARED VAR s-codcia  AS INT.
DEF SHARED VAR s-coddiv  LIKE gn-divi.coddiv.
DEF SHARED VAR s-codter  LIKE ccbcterm.codter.
DEF SHARED VAR s-user-id AS CHAR.

DEF VAR C-NomCon   AS CHAR FORMAT "X(40)".
DEF VAR X-EnLetras AS CHAR FORMAT "x(60)" NO-UNDO.
DEF VAR x-Percepcion AS CHAR FORMAT "x(60)" NO-UNDO.
DEF VAR X-Cheque   AS CHAR    NO-UNDO.
DEF VAR C-MONEDA   AS CHAR FORMAT "x(8)"  NO-UNDO.
DEF VAR N-Item     AS INTEGER NO-UNDO.
DEF VAR X-impbrt   AS DECIMAL NO-UNDO.
DEF VAR X-dscto1   AS DECIMAL NO-UNDO.
DEF VAR X-preuni   AS DECIMAL NO-UNDO.
DEF VAR C-DESALM   AS CHAR NO-UNDO FORMAT "X(40)".
DEF VAR X-PUNTOS   AS CHAR FORMAT "X(40)".
DEF VAR X-LIN1     AS CHAR FORMAT "X(40)".
DEF VAR X-LIN1A    AS CHAR FORMAT "X(40)".
DEF VAR X-LIN2     AS CHAR FORMAT "X(40)".
DEF VAR X-LIN3     AS CHAR FORMAT "X(40)".
DEF VAR X-LIN4     AS CHAR FORMAT "X(40)".
DEF VAR X-LIN4A    AS CHAR FORMAT "X(30)".
DEF VAR X-LIN5     AS CHAR FORMAT "X(40)".
DEF VAR X-LIN6     AS CHAR FORMAT "X(40)".
DEF VAR X-LIN7     AS CHAR FORMAT "X(40)".
DEF VAR X-LIN8     AS CHAR FORMAT "X(40)".
DEF VAR X-LIN9     AS CHAR FORMAT "X(40)".
DEF VAR X-LIN10    AS CHAR FORMAT "X(40)".
DEF VAR X-LIN11    AS CHAR FORMAT "X(40)".
DEF VAR X-LIN12    AS CHAR FORMAT "X(40)".
DEF VAR X-LIN13    AS CHAR FORMAT "X(40)".
DEF VAR X-MAQ      AS CHAR FORMAT "X(40)".
DEF VAR X-CANCEL   AS CHAR FORMAT "X(10)".

DEF VAR X-SOL      AS DECI INIT 0.
DEF VAR X-DOL      AS DECI INIT 0.
DEF VAR X-VUE      AS DECI INIT 0.

FIND ccbcdocu WHERE ROWID(ccbcdocu) = X-ROWID NO-LOCK NO-ERROR.
IF NOT AVAILABLE ccbcdocu THEN RETURN.

C-MONEDA = IF ccbcdocu.codmon = 1 THEN "SOLES" ELSE "DOLARES".

C-NomCon = "".


FIND FIRST Ccbddocu OF Ccbcdocu NO-LOCK NO-ERROR.
IF AVAIL CcbDdocu THEN DO:
    FIND Almacen WHERE Almacen.CodCia = ccbcdocu.codcia 
                  AND  Almacen.CodAlm = CcbDDocu.AlmDes 
                 NO-LOCK NO-ERROR.
    IF AVAILABLE Almacen THEN 
        ASSIGN C-DESALM = Almacen.CodAlm + " - " + Almacen.Descripcion.
END.
ELSE DO:
    FIND Almacen WHERE Almacen.CodCia = ccbcdocu.codcia 
                  AND  Almacen.CodAlm = ccbcdocu.codalm 
                 NO-LOCK NO-ERROR.
    IF AVAILABLE Almacen THEN 
        ASSIGN C-DESALM = Almacen.CodAlm + " - " + Almacen.Descripcion.
END.

FIND gn-ConVt WHERE gn-ConVt.Codig = CcbCDocu.FmaPgo NO-LOCK NO-ERROR.
IF AVAILABLE gn-ConVt THEN 
   ASSIGN C-NomCon = gn-ConVt.Nombr.

/************************  PUNTEROS EN POSICION  *******************************/
RUN bin/_numero(ccbcdocu.imptot, 2, 1, OUTPUT X-EnLetras).
X-EnLetras = "SON : " + X-EnLetras + (IF ccbcdocu.codmon = 1 THEN " SOLES" ELSE " DOLARES AMERICANOS").

/* ********************************* BLOQUEADO *************************************************** 
/* Verifica que la cancelación se realizó con CHEQUE */
x-cheque = "".
IF CcbCDocu.Flgest = 'C' THEN DO:
   FIND FIRST CcbDCaja WHERE CcbDCaja.CodCia = s-codcia 
                        AND  CcbDCaja.CodRef = CcbCDocu.Coddoc 
                        AND  CcbDCaja.NroRef = CcbCDocu.Nrodoc 
                       NO-LOCK NO-ERROR.
   IF AVAILABLE CcbDCaja THEN DO:
      FIND CcbCCaja OF CcbDCaja NO-LOCK NO-ERROR.
      IF AVAILABLE CcbCCaja THEN DO:
         x-cheque = TRIM(CcbCCaja.Voucher[2]) + TRIM(CcbCCaja.Voucher[3]).
         IF CcbCCaja.CodBco[2] <> '' THEN DO:
            FIND cb-tabl WHERE cb-tabl.Tabla = "04" AND cb-tabl.codigo = CcbCCaja.CodBco[2]
                         NO-LOCK NO-ERROR.
            IF AVAILABLE cb-tabl THEN
               x-cheque = x-cheque + " " + TRIM(cb-tabl.Nombre).
         END.
         IF CcbCCaja.CodBco[3] <> '' THEN DO:
            FIND cb-tabl WHERE cb-tabl.Tabla = "04" AND cb-tabl.codigo = CcbCCaja.CodBco[3]
                         NO-LOCK NO-ERROR.
            IF AVAILABLE cb-tabl THEN 
               x-cheque = x-cheque + " " + TRIM(cb-tabl.Nombre).
         END.
         IF CcbcCaja.ImpNac[1] + CcbCCaja.ImpUsa[1] > 0 Then x-cancel = x-cancel + ",Efectivo".
         IF CcbcCaja.ImpNac[2] + CcbCCaja.ImpUsa[2] + CcbcCaja.ImpNac[3] + CcbCCaja.ImpUsa[3] > 0 Then x-cancel = x-cancel + ",Cheque".
         IF CcbcCaja.ImpNac[4] + CcbCCaja.ImpUsa[4] > 0 Then x-cancel = x-cancel + ",Tarjeta".
         x-sol = CcbcCaja.ImpNac[1] + CcbcCaja.ImpNac[2] + CcbcCaja.ImpNac[3] + CcbcCaja.ImpNac[4].
         x-dol = CcbcCaja.ImpUsa[1] + CcbcCaja.ImpUsa[2] + CcbcCaja.ImpUsa[3] + CcbcCaja.ImpUsa[4].
         x-vue = CcbCCaja.VueNac .
         
      END.
   END.
END.
******************************************************************************************************* */
 
DEFINE FRAME F-HdrBol
    HEADER
    SKIP(2)
    'L A U   C H U N' AT 10 SKIP
    "OFIMAX PERU SAC"  AT 15 SKIP 
    "Av.General Canevaro 388 Of 503 Lince" AT 1 SKIP
    "Lima-Peru" AT 15 SKIP
    "Telefono : 3444444" AT 15 SKIP
    "R.U.C. 20503843812" AT 15 SKIP
    "Ticket No : " AT 1 
    ccbcdocu.nrodoc AT 15 FORMAT "XXX-XXXXXX" 
    TODAY AT 30 SKIP
    STRING(TIME,"HH:MM:SS") AT 30
    WITH PAGE-TOP NO-LABELS NO-BOX STREAM-IO WIDTH 150.

DEFINE FRAME F-DetaBol
    ccbddocu.codmat AT 1
    almmmatg.desmat AT 8   FORMAT "x(15)"
    ccbddocu.undvta AT 25  FORMAT "X(4)"
    ccbddocu.candes AT 30  FORMAT ">>>99"
    ccbddocu.implin AT 36  FORMAT ">>,>>>9.99" 
    WITH NO-LABELS NO-BOX STREAM-IO WIDTH 150.

/*MLR* 28/12/07 ***/
DEF VAR cNroIC LIKE CcbDCaja.Nrodoc NO-UNDO.

/* Verifica que la cancelación se realizó con CHEQUE */
x-cheque = "".
IF CcbCDocu.Flgest = 'C' THEN DO:
/*MLR* 28/12/07 ***
   FIND FIRST CcbDCaja WHERE CcbDCaja.CodCia = s-codcia 
                        AND  CcbDCaja.CodRef = CcbCDocu.Coddoc 
                        AND  CcbDCaja.NroRef = CcbCDocu.Nrodoc 
                       NO-LOCK NO-ERROR.
   IF AVAILABLE CcbDCaja THEN DO:
      FIND CcbCCaja OF CcbDCaja NO-LOCK NO-ERROR.
      IF AVAILABLE CcbCCaja THEN DO:
* ***/
/*MLR* 28/12/07 ***/
    FOR EACH CcbDCaja WHERE
        CcbDCaja.CodCia = s-codcia AND
        CcbDCaja.CodRef = CcbCDocu.Coddoc AND
        CcbDCaja.NroRef = CcbCDocu.Nrodoc NO-LOCK,
        FIRST ccbccaja WHERE
        ccbccaja.codcia = ccbdcaja.codcia AND
        ccbccaja.coddiv = ccbdcaja.coddiv AND
        ccbccaja.coddoc = ccbdcaja.coddoc AND
        ccbccaja.nrodoc = ccbdcaja.nrodoc NO-LOCK:
        cNroIC = CcbDCaja.Nrodoc.

         x-cheque = TRIM(CcbCCaja.Voucher[2]) + TRIM(CcbCCaja.Voucher[3]).
         IF CcbCCaja.CodBco[2] <> '' THEN DO:
            FIND cb-tabl WHERE cb-tabl.Tabla = "04" AND cb-tabl.codigo = CcbCCaja.CodBco[2]
                         NO-LOCK NO-ERROR.
            IF AVAILABLE cb-tabl THEN
               x-cheque = x-cheque + " " + TRIM(cb-tabl.Nombre).
         END.
         IF CcbCCaja.CodBco[3] <> '' THEN DO:
            FIND cb-tabl WHERE cb-tabl.Tabla = "04" AND cb-tabl.codigo = CcbCCaja.CodBco[3]
                         NO-LOCK NO-ERROR.
            IF AVAILABLE cb-tabl THEN 
               x-cheque = x-cheque + " " + TRIM(cb-tabl.Nombre).
         END.
         IF CcbcCaja.ImpNac[1] + CcbCCaja.ImpUsa[1] > 0 Then x-cancel = x-cancel + ",Efectivo".
         IF CcbcCaja.ImpNac[2] + CcbCCaja.ImpUsa[2] + CcbcCaja.ImpNac[3] + CcbCCaja.ImpUsa[3] > 0 Then x-cancel = x-cancel + ",Cheque".
         IF CcbcCaja.ImpNac[4] + CcbCCaja.ImpUsa[4] > 0 Then x-cancel = x-cancel + ",Tarjeta".
         x-sol = CcbcCaja.ImpNac[1] + CcbcCaja.ImpNac[2] + CcbcCaja.ImpNac[3] + CcbcCaja.ImpNac[4].
         x-dol = CcbcCaja.ImpUsa[1] + CcbcCaja.ImpUsa[2] + CcbcCaja.ImpUsa[3] + CcbcCaja.ImpUsa[4].
         x-vue = CcbCCaja.VueNac .
/*MLR* 28/12/07 ***         
      END.
* ***/
   END.
END.

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
         HEIGHT             = 5.08
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


/* ******************************************************************** */
/* IMPRESORA START MICRONICS                                            */
/* ***************************  Main Block  *************************** */

DEF VAR x-Tienda AS CHAR FORMAT 'x(50)'.
DEF VAR x-Lineas AS INT NO-UNDO INIT 0.
DEF VAR x-size   AS INT NO-UNDO INIT 0.
DEF VAR iInt     AS INT NO-UNDO INIT 1.
DEF VAR x-lin-fin AS CHAR NO-UNDO FORMAT 'X(30)'.
DEF VAR x-lin10b  AS CHARACTER   NO-UNDO FORMAT 'X(30)'.
DEF VAR x-lin-ruc AS CHARACTER   NO-UNDO FORMAT 'X(30)'.
DEF VAR x-lin-dir AS CHARACTER   NO-UNDO FORMAT 'X(30)'.
DEF VAR x-lin-dir1 AS CHARACTER   NO-UNDO FORMAT 'X(30)'.
DEF VAR x-lin-dir2 AS CHARACTER   NO-UNDO FORMAT 'X(30)'.
DEF VAR x-num-tel AS CHARACTER   NO-UNDO.
DEF VAR x-dsctoE  AS CHARACTER   NO-UNDO.
DEF VAR x-valigv  AS CHARACTER   NO-UNDO.
DEF VAR x-codbrr  AS CHARACTER   NO-UNDO FORMAT 'X(60)'.
DEF VAR x-CuentaPremium AS INT NO-UNDO.   /* Cuenta ofertas PREMIUM (Libre_c04) */

FIND FIRST ccbdterm WHERE 
     CcbDTerm.CodCia = S-CodCia AND
     CcbDTerm.CodDoc = CcbCDocu.CodDoc AND
     CcbDTerm.CodDiv = S-CodDiv AND
     CcbDTerm.NroSer = INTEGER(SUBSTRING(CcbCDocu.NroDoc, 1, 3)) NO-LOCK.
     
FIND FacCorre WHERE 
     FacCorre.CodCia = S-CODCIA AND 
     FacCorre.CodDiv = S-CODDIV AND
     FacCorre.CodDoc = ccbcdocu.coddoc AND
     FacCorre.NroSer = CcbDTerm.NroSer NO-LOCK.

RUN lib/_port-name (FacCorre.Printer, OUTPUT s-port-name).
IF s-port-name = '' THEN RETURN.

x-Maq = faccorre.nroimp.
x-lin4A = ''.
/* Detalle */
x-lin3      = "Term/Usr :" + TRIM(S-CODTER) + "/" + SUBSTRING(TRIM(Ccbcdocu.Usuario),1,7) + "      " + STRING(TIME,"HH:MM:SS").
x-lin4      = "T O T A L     : S/" + STRING((Ccbcdocu.ImpTot),">>>,>>9.99").
x-lin5      = "I/C,P/M,ALM:" + cNroIC + "/" + Ccbcdocu.NroPed + "/" + Ccbcdocu.Nrosal .

CASE Ccbcdocu.CodDiv:
    /*WHEN '00001' THEN x-Maq = '150230600166'.       /* Ucayali */*/
    /*WHEN '00002' THEN x-Maq = '150230900025'.       /* Andahuaylas */*/
    WHEN '00003' THEN x-Maq = '150230600225'.       /* Paruro */
END.

IF ccbcdocu.libre_c04 = 'FAC' THEN DO :
    x-lin7    = "Razon Soc.: " + ccbcdocu.nomcli.
    x-lin-ruc = "RUC : " + ccbcdocu.ruccli.    
    x-lin9    = "Factura No: " + STRING(ccbcdocu.nrodoc,"XXX-XXXXXX") + "     " + STRING(ccbcdocu.fchdoc,'99/99/9999').
END.
ELSE DO: 
    x-lin7 = "Cliente: " + ccbcdocu.nomcli.    
    x-lin9 = "Ticket No : " + STRING(ccbcdocu.nrodoc,"XXX-XXXXXX") + "     " + STRING(ccbcdocu.fchdoc,'99/99/9999').
END.



/* IF ccbcdocu.fchdoc < 03/01/2011 THEN x-valigv = '19'. */
/* ELSE x-valigv = '18'.                                 */
x-valigv = STRING(CcbCDocu.PorIgv, ">9").

x-lin6 = "Direccion: " + ccbcdocu.dircli.
x-lin8 = "Maq. Registradora : " + x-maq. 
x-lin10  = "     Efectivo  S/ :" + STRING(X-SOL,">>>,>>9.99").
x-lin10b = "     Efectivo US$.:" + STRING(X-DOL,">>>,>>9.99").
x-lin11  = "        Vuelto S/ :" + STRING(X-VUE,">>>,>>9.99") .
x-lin12 = "".
x-lin13 = "".
IF (Ccbcdocu.fchdoc >= 07/01/2013) AND Ccbcdocu.acubon[5] > 0 
    THEN ASSIGN
    /*
      PRECIO TOTAL  : S/.>>>,>>9.99
      PERCEPCION >9.99%: >>>,>>9.99
      TOTAL COBRADO : S/.>>>,>>9.99
    */
    x-lin4  = "PRECIO TOTAL  : S/" + STRING((Ccbcdocu.ImpTot - Ccbcdocu.AcuBon[5]),">>>,>>9.99")
    x-lin12 = "PERCEPCION " + STRING(Ccbcdocu.acubon[4], '>9.99') + "%: " +
                    STRING(Ccbcdocu.acubon[5], '>>>,>>9.99')
    x-lin13 = "TOTAL COBRADO : S/" + STRING(Ccbcdocu.ImpTot, ">>>,>>9.99")
    x-Percepcion = '* Operación sujeta a percepción del IGV'.

x-lin-fin = "IGV " + x-valigv + "% S/ " + STRING(CcbCDocu.ImpIgv,">,>>9.99").

FIND Gn-Divi WHERE Gn-Divi.codcia = Ccbcdocu.codcia
    AND Gn-Divi.coddiv = Ccbcdocu.coddiv
    NO-LOCK NO-ERROR.
IF AVAILABLE Gn-Divi THEN 
    ASSIGN 
        x-Tienda = CAPS(GN-DIVI.DesDiv)
        x-lin-dir = SUBSTRING(GN-DIVI.DirDiv,1,50)
        x-lin-dir1= SUBSTRING(GN-DIVI.DirDiv,51)
        x-lin-dir2= GN-DIVI.FaxDiv
        x-num-tel = 'Telf: ' + gn-divi.teldiv.

/*Calcula Promociones*/
/*RUN Calcula-Promociones.*/

/* CONTAMOS CUANTAS LINEAS TIENE LA IMPRESION */
FOR EACH ccbddocu OF ccbcdocu NO-LOCK:
    x-Lineas = x-Lineas + 1.
END.
/* AGREGAMOS LOS TITULOS Y PIE DE PAGINA */
x-Lineas = x-Lineas + 27.

IF s-OpSys = 'WinVista'
    THEN OUTPUT TO PRINTER VALUE(s-port-name).
ELSE OUTPUT TO VALUE(s-port-name).

PUT CONTROL Chr(27) + Chr(112) + Chr(48) .
PUT CONTROL {&PRN0} + {&PRN5A} + CHR(x-Lineas) + {&PRN4}.
/*PUT CONTROL {&Prn0} + {&Prn5A} + CHR(33) + {&Prn4} .*/
DISPLAY 
    {&PRN3} + 'CONTINENTAL S.A.C.' NO-LABEL AT 12 FORMAT "X(60)" SKIP
    "Especialista en utiles " AT 10 FORMAT "X(50)" SKIP 
    "escolares y de oficina" + {&PRN4} AT 10 FORMAT "X(50)" SKIP 
    x-Tienda AT 1 FORMAT "X(50)" NO-Label SKIP
    x-lin-dir AT 1 FORMAT "X(75)" NO-LABEL SKIP
    x-lin-dir1 WHEN x-lin-dir1 <> '' AT 1 FORMAT "X(75)" NO-LABEL  SKIP
    x-lin-dir2 AT 1 FORMAT "X(75)" NO-LABEL SKIP
    x-num-tel NO-LABEL AT 1 FORMAT 'X(15)' " R.U.C. 20100038146" AT 20 FORMAT "X(20)" SKIP
    x-lin9 NO-LABEL SKIP
    x-lin8 No-LABEL SKIP
    x-lin3 NO-LABEL SKIP
    x-lin7 NO-LABEL SKIP
    x-lin-ruc NO-LABEL.     

PUT "--------------------------------------" SKIP.

x-lin1A = ''.
FOR EACH ccbddocu OF ccbcdocu NO-LOCK , 
        FIRST almmmatg OF ccbddocu NO-LOCK
        BREAK BY ccbddocu.nrodoc 
        BY ccbddocu.NroItm:
        x-lin1 = TRIM(ccbddocu.codmat) + " " + 
            SUBSTRING(TRIM(almmmatg.desmat),1,25) + " " + 
            SUBSTRING(TRIM(almmmatg.desmar),1,6).
        x-lin2 = SUBSTRING(TRIM(ccbddocu.undvta),1,6) + " " + 
            STRING(ccbddocu.candes,">,>>9.99") + " " + 
            STRING(ccbddocu.preuni,">>>9.99") + " " + 
            STRING(ccbddocu.implin,">>>,>>9.99").
        IF ccbddocu.impdcto_adelanto[5] > 0 THEN x-lin2 = x-lin2 + "*".
    DISPLAY x-lin1 NO-LABEL 
            x-lin2 NO-LABEL.
END.
PUT UNFORMATTED SKIP.
IF x-lin4A <> "" THEN PUT x-lin4A   AT 10 .
PUT x-lin4   AT 10 SKIP.
IF x-lin12 <> "" THEN PUT x-lin12  AT 10 SKIP.
IF x-lin13 <> "" THEN PUT x-lin13  AT 10 SKIP.
PUT x-lin10  AT 10 .
IF x-lin10b <> '' THEN PUT x-lin10b AT 10 SKIP.
IF x-lin11 <> '' THEN PUT x-lin11  AT 10 SKIP(2).
IF x-CuentaPremium > 0 THEN PUT UNFORMATTED 'CUADERNOS PREMIUM ' STRING(x-CuentaPremium, '>>9') SKIP.
PUT x-lin-fin  SKIP .
IF x-dsctoE <> '' THEN PUT x-dsctoE FORMAT 'X(40)' SKIP. 
IF ccbcdocu.acubon[5] > 0 THEN
    PUT x-Percepcion SKIP.
PUT "--------------------------------------" SKIP.
PUT "NO SE ACEPTAN CAMBIOS NI DEVOLUCIONES" SKIP.
/*RD01 - Detalle de Promocion*/
IF CcbCDocu.Libre_c05 <> '' THEN DO:
    IF INT(LENGTH(CcbCDocu.Libre_c05)) MOD 40 > 0 
        THEN x-size = TRUNCATE(INT(LENGTH(CcbCDocu.Libre_c05)) / 40,0) + 1.
    ELSE x-size = TRUNCATE(INT(LENGTH(CcbCDocu.Libre_c05)) / 40,0).

    IF x-size > 0 THEN DO:
        PUT "PROMOCION" AT 15 SKIP.
        DO iInt = 1 TO x-size:
            PUT SUBSTRING(CcbcDocu.Libre_c05,((iInt - 1) * 40 + 1),(iInt * 40)) AT 01 FORMAT "X(40)" SKIP.
        END.
    END.
END.
     
PUT "GRACIAS POR SU COMPRA" AT 12 SKIP.

IF pTipo = "C" THEN DO:
    PUT UNFORMATTED {&PRN7A} + "C O P I A" + {&PRN7B} AT 1 SKIP.
END.

PUT "STANDFORD - CONTI" AT 14 SKIP(5).
PUT CONTROL CHR(27) + 'm'.

OUTPUT CLOSE.

RUN Graba-Log.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-copia-mainblock) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE copia-mainblock Procedure 
PROCEDURE copia-mainblock :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/* ******************************************************************** */
/* IMPRESORA START MICRONICS                                            */
/* ***************************  Main Block  *************************** */

DEF VAR x-Tienda AS CHAR FORMAT 'x(50)'.
DEF VAR x-Direc1 AS CHAR FORMAT 'x(50)'.
DEF VAR x-Direc2 AS CHAR FORMAT 'x(50)'.
DEF VAR x-Num-Tel AS CHAR.
DEF VAR x-Lineas AS INT NO-UNDO INIT 0.
DEF VAR x-size   AS INT NO-UNDO INIT 0.
DEF VAR iInt     AS INT NO-UNDO INIT 1.

FIND ccbdterm WHERE 
     CcbDTerm.CodCia = S-CodCia AND
     CcbDTerm.CodDoc = CcbCDocu.CodDoc AND
     CcbDTerm.CodDiv = S-CodDiv AND
     CcbDTerm.CodTer = S-CodTer AND
     CcbDTerm.NroSer = INTEGER(SUBSTRING(CcbCDocu.NroDoc, 1, 3)) NO-LOCK.
     
FIND FacCorre WHERE 
     FacCorre.CodCia = S-CODCIA AND 
     FacCorre.CodDiv = S-CODDIV AND
     FacCorre.CodDoc = ccbcdocu.coddoc AND
     FacCorre.NroSer = CcbDTerm.NroSer NO-LOCK.

RUN lib/_port-name (FacCorre.Printer, OUTPUT s-port-name).
IF s-port-name = '' THEN RETURN.

x-Maq = faccorre.nroimp.

/* Detalle */
x-lin3 = "Term/Usr :" + TRIM(S-CODTER) + "/" + SUBSTRING(TRIM(Ccbcdocu.Usuario),1,7) + "      " + STRING(TIME,"HH:MM:SS").
x-lin4 = "Total Importe : S/." + STRING(Ccbcdocu.ImpTot,">>>,>>9.99").
x-lin5 = "I/C,P/M,ALM:" + cNroIC + "/" + Ccbcdocu.NroPed + "/" + Ccbcdocu.Nrosal .

CASE Ccbcdocu.CodDiv:
    /*WHEN '00001' THEN x-Maq = '150230600166'.       /* Ucayali */*/
    /*WHEN '00002' THEN x-Maq = '150230900025'.       /* Andahuaylas */*/
    WHEN '00003' THEN x-Maq = '150230600225'.       /* Paruro */
END.

x-lin7 = "  Cliente: " + ccbcdocu.nomcli.
x-lin6 = "Direccion: " + ccbcdocu.dircli.
x-lin8 = "Maq. Registradora : " + x-maq. 
x-lin9 = "Pago : " + x-Cancel .
x-lin10 = "  S/." + STRING(X-SOL,">>,>>9.99") + "    US$" + STRING(X-DOL,">>,>>9.99").
x-lin11 = "Vuelto S/." + STRING(X-VUE,">>,>>9.99") .
x-lin12 = "".
x-lin13 = "".
IF (LOOKUP(s-user-id, 'ADMIN,PRUEBA') > 0 OR Ccbcdocu.fchdoc >= 07/01/2013) AND Ccbcdocu.acubon[5] > 0 
    THEN ASSIGN
             /*      PRECIO TOTAL: S/.>>>,>>9.99
               IMP. PERCEPCION >9.99% >>>,>>9.99
                      MONTO TOTAL: S/.>>>,>>9.99
             */
    x-lin4  = "      PRECIO TOTAL: S/." + STRING((Ccbcdocu.ImpTot - Ccbcdocu.acubon[5]), ">>>,>>9.99")
    x-lin12 = "IMP. PERCEPCION " + STRING(Ccbcdocu.acubon[4], '>9.99') + "% " +
                STRING(Ccbcdocu.acubon[5], '>>>,>>9.99')
    x-lin13 = "     TOTAL COBRADO: S/." + STRING(Ccbcdocu.ImpTot, ">>>,>>9.99")
    x-Percepcion = '* Operación sujeta a percepción del IGV'.
FIND Gn-Divi WHERE Gn-Divi.codcia = Ccbcdocu.codcia
    AND Gn-Divi.coddiv = Ccbcdocu.coddiv
    NO-LOCK NO-ERROR.
IF AVAILABLE Gn-Divi THEN 
    ASSIGN
        x-Tienda = GN-DIVI.DesDiv
        x-Direc1 = SUBSTRING(GN-DIVI.DirDiv,1,50)
        x-Direc2 = SUBSTRING(GN-DIVI.DirDiv,51)
        x-Num-Tel =  GN-DIVI.TelDiv.

/* CONTAMOS CUANTAS LINEAS TIENE LA IMPRESION */
FOR EACH ccbddocu OF ccbcdocu NO-LOCK:
    x-Lineas = x-Lineas + 1.
END.
/* AGREGAMOS LOS TITULOS Y PIE DE PAGINA */
x-Lineas = x-Lineas + 27.

IF s-OpSys = 'WinVista'
THEN OUTPUT TO PRINTER VALUE(s-port-name).
ELSE OUTPUT TO VALUE(s-port-name).

/*PUT CONTROL {&PRN0} + {&PRN5A} + CHR(36) + {&PRN4}.*/
PUT CONTROL {&PRN0} + {&PRN5A} + CHR(x-Lineas) + {&PRN4}.

/*RDP - Comando para abrir caja registradora*/
PUT CONTROL Chr(27) + Chr(112) + Chr(48) .
DISPLAY 
    'CONTINENTAL S.A.C.' AT 12 FORMAT "X(60)" SKIP
    "Especialista en utiles " AT 10 FORMAT "X(50)" SKIP 
    "escolares y de oficina" AT 10 FORMAT "X(50)" SKIP 
    x-Tienda AT 1 FORMAT "X(50)" NO-LABEL SKIP
    x-Direc1 AT 1 FORMAT 'x(50)' NO-LABEL SKIP
    x-Direc2 AT 1 FORMAT 'x(50)' NO-LABEL SKIP
    x-num-tel NO-LABEL AT 1 FORMAT 'X(15)' " R.U.C. 20100038146" AT 20 FORMAT "X(20)" SKIP
    "Ticket No : " AT 1 ccbcdocu.nrodoc AT 15 FORMAT "XXX-XXXXXX" NO-LABEL ccbcdocu.fchdoc AT 30 NO-LABEL SKIP
    x-lin7 No-LABEL SKIP
    x-lin6 No-LABEL SKIP
    x-lin8 No-LABEL SKIP
    x-lin5 NO-LABEL SKIP    
    x-lin3 NO-LABEL SKIP
    x-lin4 NO-LABEL SKIP
    x-lin12 NO-LABEL SKIP
    x-lin13 NO-LABEL SKIP
    x-lin9 NO-LABEL SKIP
    x-lin10 NO-LABEL SKIP
    x-lin11 NO-LABEL.     

FOR EACH ccbddocu OF ccbcdocu NO-LOCK , 
        FIRST almmmatg OF ccbddocu NO-LOCK
        BREAK BY ccbddocu.nrodoc 
        BY ccbddocu.NroItm:
        x-lin1 = TRIM(ccbddocu.codmat) + " " + 
            SUBSTRING(TRIM(almmmatg.desmat),1,25) + " " + 
            SUBSTRING(TRIM(almmmatg.desmar),1,6).
        x-lin2 = SUBSTRING(TRIM(ccbddocu.undvta),1,6) + " " + 
            STRING(ccbddocu.candes,">,>>9.99") + " " + 
            STRING(ccbddocu.preuni,">>>9.99") + " " + 
            STRING(ccbddocu.implin,">>>,>>9.99").
        IF ccbddocu.impdcto_adelanto[5] > 0 THEN x-lin2 = x-lin2 + "*".
    DISPLAY x-lin1 NO-LABEL 
            x-lin2 NO-LABEL.
END.
IF ccbcdocu.acubon[5] > 0 THEN
    PUT x-Percepcion SKIP.
PUT "--------------------------------------" SKIP.
PUT "NO SE ACEPTAN CAMBIOS NI DEVOLUCIONES" SKIP.
/*RD01 - Detalle de Promocion*/
IF INT(LENGTH(CcbCDocu.Libre_c05)) MOD 40 > 0 
    THEN x-size = TRUNCATE(INT(LENGTH(CcbCDocu.Libre_c05)) / 40,0) + 1.
ELSE x-size = TRUNCATE(INT(LENGTH(CcbCDocu.Libre_c05)) / 40,0).

IF x-size > 0 THEN DO:
    PUT "PROMOCION" AT 15 SKIP.
    DO iInt = 1 TO x-size:
        PUT SUBSTRING(CcbcDocu.Libre_c05,((iInt - 1) * 40 + 1),(iInt * 40)) AT 01 FORMAT "X(40)" SKIP.
    END.
END.


/*PUT CcbcDocu.Libre_c05 AT 01 FORMAT "X(40)" SKIP.*/
PUT "GRACIAS POR SU COMPRA" AT 12 SKIP.
PUT "STANDFORD - CONTI" AT 14 SKIP(5).
/*PUT CONTROL CHR(27) + 'F'.  /*&& Corte de hoja*/*/
/*PUT CONTROL CHR(27) "d" "3".*/
PUT CONTROL CHR(27) + 'm'.

OUTPUT CLOSE.




/* ************************* RUTINA ANTERIOR *************************
/* Detalle */
x-lin3 = "Term/Usr :" + TRIM(S-CODTER) + "/" + SUBSTRING(TRIM(Ccbcdocu.Usuario),1,7) + "      " + STRING(TIME,"HH:MM:SS").
x-lin4 = "Total Importe : S/." + STRING(Ccbcdocu.ImpTot,">>>,>>9.99").
x-lin5 = "I/C,P/M,ALM:" + CcbDCaja.Nrodoc + "/" + Ccbcdocu.NroPed + "/" + Ccbcdocu.Nrosal .

CASE Ccbcdocu.CodDiv:
    WHEN '00001' THEN x-Maq = '150230600166'.       /* Ucayali */
    WHEN '00002' THEN x-Maq = '150230900025'.       /* Andahuaylas */
    WHEN '00003' THEN x-Maq = '150230600225'.       /* Paruro */
END.

x-lin7 = "  Cliente: " + ccbcdocu.nomcli.
x-lin6 = "Direccion: " + ccbcdocu.dircli.
x-lin8 = "Maq. Registradora : " + x-maq. 
x-lin9 = "Pago : " + X-CANCEl .
x-lin10 = "  S/." + STRING(X-SOL,">>,>>9.99") + "    US$" + STRING(X-DOL,">>,>>9.99").
x-lin11 = "Vuelto S/." + STRING(X-VUE,">>,>>9.99") .
 
DISPLAY 
    "Continental S.A.C." AT 12 FORMAT "X(60)" SKIP
    "Especialista en utiles " AT 10 FORMAT "X(50)" SKIP 
    "escolares y de oficina" AT 10 FORMAT "X(50)" SKIP 
    "Jr. Miroquesada 635 - Lima" AT 1 FORMAT "X(50)" SKIP
    "Calle Renee Descartes Mz.C LT.1 " AT 1 FORMAT "X(50)" SKIP
    "Ate - Vitarte " AT 14 FORMAT "X(50)" SKIP
    "Telf: 427-6473     R.U.C. 20100038146" AT 1 FORMAT "X(40)" SKIP
    "Ticket No : " AT 1 ccbcdocu.nrodoc AT 15 FORMAT "XXX-XXXXXX" NO-LABEL TODAY AT 30 SKIP
    x-lin7 No-LABEL SKIP
    x-lin6 No-LABEL SKIP
    x-lin8 No-LABEL SKIP
    x-lin5 NO-LABEL SKIP    
    x-lin3 NO-LABEL SKIP
    x-lin4 NO-LABEL SKIP
    x-lin9 NO-LABEL SKIP
    x-lin10 NO-LABEL SKIP
    x-lin11 NO-LABEL.     

FOR EACH ccbddocu OF ccbcdocu NO-LOCK , 
        FIRST almmmatg OF ccbddocu NO-LOCK
        BREAK BY ccbddocu.nrodoc 
        BY ccbddocu.NroItm:
        x-lin1 = TRIM(ccbddocu.codmat) + " " + SUBSTRING(TRIM(almmmatg.desmat),1,25) + " " + SUBSTRING(TRIM(almmmatg.desmar),1,6).
        x-lin2 = SUBSTRING(TRIM(ccbddocu.undvta),1,6) + " " + STRING(ccbddocu.candes,">,>>9.99") + " " + STRING(ccbddocu.preuni,">>>9.99") + " " + STRING(ccbddocu.implin,">>>,>>9.99").
    DISPLAY x-lin1 NO-LABEL 
            x-lin2 NO-LABEL.
END.
PUT "--------------------------------------" SKIP.
PUT "GRACIAS POR SU COMPRA" AT 12 SKIP.
PUT "STANDFORD - CONTI" AT 14 SKIP.
/*PUT CONTROL CHR(27) + 'F'.  /*&& Corte de hoja*/*/
PUT CONTROL CHR(27) "d" "3".
*/


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Graba-Log) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Graba-Log Procedure 
PROCEDURE Graba-Log :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

IF pTipo = "C" THEN DO:
    RUN lib/logtabla ("ccbcdocu",ccbcdocu.coddoc + '|' + ccbcdocu.nrodoc, "REIMPRESION").
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

