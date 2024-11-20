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

DEF VAR x-CodFchF AS DATE NO-UNDO.
DEF VAR x-CodFchI AS DATE NO-UNDO.
DEF VAR s-CodCia AS INT INIT 001 NO-UNDO.

ASSIGN
    x-CodFchF = TODAY - 1
    x-CodFchI = DATE(01, 01, YEAR(TODAY)).      /* Por defecto */
/* Fecha de Cierre */
DEF VAR dFchCie AS DATE NO-UNDO.
IF DAY(TODAY) < 15 
THEN dFchCie = TODAY - DAY(TODAY).
ELSE dFchCie = TODAY.
dFchCie = dFchCie - DAY(dFchCie) + 1. 
x-CodFchI = ADD-INTERVAL(dFchCie, -1, "months").

x-CodFchI = ADD-INTERVAL(x-CodFchF, -15, "days").

/* Clientes */
DEF TEMP-TABLE t-cliente
    FIELD codcli        LIKE    gn-clie.codcli       
    FIELD nomcli        LIKE    gn-clie.nomcli      
    FIELD CodDept       LIKE    gn-clie.CodDept     
    FIELD CodProv       LIKE    gn-clie.CodProv     
    FIELD CodDist       LIKE    gn-clie.CodDist     
    FIELD dirent        LIKE    gn-clie.dirent      
    FIELD contac        LIKE    gn-clie.contac      
    FIELD ruc           LIKE    gn-clie.ruc         
    /*FIELD referencias   LIKE    gn-clie.referencias */
    FIELD gircli        LIKE    gn-clie.gircli      
    FIELD codven        LIKE    gn-clie.codven      
    FIELD cndvta        LIKE    gn-clie.cndvta      
    FIELD telfnos       LIKE    gn-clie.telfnos     
    FIELD coddiv        LIKE    gn-clie.coddiv      
    FIELD canal         LIKE    gn-clie.canal       
    FIELD nrocard       LIKE    gn-clie.nrocard     
    FIELD codunico      LIKE    gn-clie.codunico    
    FIELD dni           LIKE    gn-clie.dni         
    FIELD apepat        LIKE    gn-clie.apepat      
    FIELD apemat        LIKE    gn-clie.apemat      
    FIELD nombre        LIKE    gn-clie.nombre      
    FIELD clfcli        LIKE    gn-clie.clfcli      
    FIELD clfcli2       LIKE    gn-clie.clfcli2           
    FIELD exceptua      AS LOG                      
    FIELD rucold        LIKE    gn-clie.rucold      
    FIELD libre_l01     LIKE    gn-clie.libre_l01   
    FIELD flgsit        LIKE    gn-clie.flgsit      
    FIELD e-mail        LIKE    gn-clie.e-mail      
    FIELD e-maile       LIKE    gn-clie.e-mail      
    FIELD dircli        LIKE    gn-clie.dircli      
    .
DEF TEMP-TABLE t-gn-clied
    FIELD codcli LIKE gn-clied.codcli
    FIELD coddept LIKE gn-clied.coddept
    FIELD codprov LIKE gn-clied.codprov
    FIELD coddist LIKE gn-clied.coddist
/*     FIELD dirent  LIKE gn-clied.dirent */
    FIELD codpos LIKE gn-clied.codpos
    FIELD FchCreacion LIKE gn-clied.fchcreacion
    FIELD UsrCreacion LIKE gn-clied.usrcreacion
    FIELD FchModificacion LIKE gn-clied.fchmodificacion
    FIELD UsrModificacion LIKE gn-clied.usrmodificacion
    FIELD Sede LIKE Gn-ClieD.Sede
    FIELD dircli AS CHAR FORMAT 'x(120)'
    .


/* prueba */
/* ASSIGN                                                     */
/*     x-CodFchF = TODAY - 1                                  */
/*     x-CodFchI = DATE(01, 01, 2016).      /* Por defecto */ */

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
         HEIGHT             = 5.5
         WIDTH              = 49.43.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
DEF STREAM Reporte.
DEF VAR x-Archivo AS CHAR NO-UNDO.

PUT UNFORMATTED 'INICIO: ' NOW SKIP.
/* */
DEF TEMP-TABLE t-Ventas_Cabecera LIKE Ventas_Cabecera
    FIELD CodRef AS CHAR FORMAT 'x(8)'
    FIELD NroRef AS CHAR FORMAT 'x(15)'.
DEF TEMP-TABLE t-Ventas_Detalle LIKE Ventas_Detalle
    FIELD CodRef AS CHAR FORMAT 'x(8)'
    FIELD NroRef AS CHAR FORMAT 'x(15)'.
DEF BUFFER NCREDITO FOR Ccbcdocu.
EMPTY TEMP-TABLE t-Ventas_Cabecera.
FOR EACH Ventas_Cabecera NO-LOCK WHERE Ventas_Cabecera.DateKey >= x-CodFchI AND
    Ventas_Cabecera.DateKey <= x-CodFchF:
    CREATE t-Ventas_Cabecera.
    BUFFER-COPY Ventas_Cabecera TO t-Ventas_Cabecera.
    IF Ventas_Cabecera.CodDoc = "N/C"  THEN DO:
        FIND NCREDITO WHERE NCREDITO.codcia = s-codcia AND
            NCREDITO.coddoc = Ventas_Cabecera.CodDoc AND
            NCREDITO.nrodoc = Ventas_Cabecera.NroDoc NO-LOCK NO-ERROR.
        IF AVAILABLE NCREDITO THEN DO:
            t-Ventas_Cabecera.CodRef = NCREDITO.CodRef.
            t-Ventas_Cabecera.NroRef = NCREDITO.NroRef.
        END.
    END.
END.
x-Archivo = "/home/v/IN/dbs/" + "ventas_cabecera" + STRING(DAY(x-CodFchI),'99') + 
    STRING(MONTH(x-CodFchI),'99') + STRING(YEAR(x-CodFchI),'9999') + ".txt".
OUTPUT TO VALUE(x-archivo) KEEP-MESSAGES.
FOR EACH t-Ventas_Cabecera NO-LOCK:
    EXPORT DELIMITER "~029" t-ventas_cabecera 
        EXCEPT t-Ventas_Cabecera.ImpNacSIGV 
        t-Ventas_Cabecera.ImpNacCIGV 
        t-Ventas_Cabecera.ImpExtSIGV 
        t-Ventas_Cabecera.ImpExtCIGV.
END.
OUTPUT CLOSE.
/* ********************************************************************************* */
/* VENTAS DETALLE */
/* ********************************************************************************* */
EMPTY TEMP-TABLE t-Ventas_Detalle.
FOR EACH t-Ventas_Cabecera NO-LOCK,
    EACH Ventas_Detalle OF t-Ventas_Cabecera NO-LOCK:
    CREATE t-Ventas_Detalle.
    BUFFER-COPY Ventas_Detalle TO t-Ventas_Detalle
        ASSIGN
        t-Ventas_Detalle.CodRef = t-Ventas_Cabecera.CodRef
        t-Ventas_Detalle.NroRef = t-Ventas_Cabecera.NroRef.
END.
x-Archivo = "/home/v/IN/dbs/" + "ventas_detalle" + STRING(DAY(x-CodFchI),'99') + 
    STRING(MONTH(x-CodFchI),'99') + STRING(YEAR(x-CodFchI),'9999') + ".txt".
OUTPUT TO VALUE(x-archivo) KEEP-MESSAGES.
FOR EACH t-Ventas_Detalle NO-LOCK:
    EXPORT delimiter "~029" t-Ventas_Detalle.
END.
OUTPUT CLOSE.
/* */
x-Archivo = "/home/v/IN/dbs/" + "almacen_stocks" + STRING(DAY(x-CodFchI),'99') + 
    STRING(MONTH(x-CodFchI),'99') + STRING(YEAR(x-CodFchI),'9999') + ".txt".
x-Archivo = "/home/v/IN/dbs/" + "almacen_stocks.txt".
OUTPUT TO VALUE(x-archivo) KEEP-MESSAGES.
FOR EACH Almacen_Stocks NO-LOCK:
    EXPORT delimiter "~029" Almacen_Stocks.
END.
OUTPUT CLOSE.

/* ******************************************************************************** */
/* CLIENTES */
/* ******************************************************************************** */
RUN Clientes.
/* ******************************************************************************** */

/* CatalE gn-clie.codunicoogo */

DEFINE VAR x-tipo-cambio AS DEC INIT 1.

FIND LAST gn-tcmb WHERE gn-tcmb.fecha <= TODAY NO-LOCK NO-ERROR.

/* 
    Ic - 24Jun2019, Por requerimiento de Max Ramos el costo de reposicion (ctolis)
    debe exportarse en Soles, debe multiplicarse con el tipo de cambio del dia que se
    esta emitiendo el reporte, ademas se debe tomar el tipo de cambio compra 
    
    Ojo : El tipo de cambio nose esta cogiendo del maestro de articulos, si no del tipo
          de cambio diario (gn-tcmb)
*/
IF AVAILABLE gn-tcmb THEN x-tipo-cambio = gn-tcmb.compra.

DEF TEMP-TABLE Detalle NO-UNDO
    FIELD codmat LIKE almmmatg.codmat
    FIELD desmat AS CHAR FORMAT 'x(100)'
    FIELD codpro AS CHAR FORMAT 'x(11)'
    FIELD licencia AS CHAR FORMAT 'x(30)'
    FIELD licenciatario AS CHAR FORMAT 'x(30)'
    FIELD monvta LIKE almmmatg.monvta
    FIELD rank_gral_c AS CHAR FORMAT 'x'
    FIELD rank_utilex_c AS CHAR FORMA 'x'
    FIELD rank_mayorista_c AS CHAR FORMAT 'x'
    FIELD rank_gral_nc AS CHAR FORMAT 'x'
    FIELD rank_utilex_nc AS CHAR FORMA 'x'
    FIELD rank_mayorista_nc AS CHAR FORMAT 'x'
    FIELD undstk LIKE almmmatg.undstk
    FIELD tpocmb LIKE almmmatg.tpocmb
    FIELD codfam LIKE almmmatg.codfam
    FIELD subfam LIKE almmmatg.subfam
    FIELD desmar AS CHAR FORMAT 'x(20)'
    FIELD ctolis AS DEC 
    FIELD peso   AS DEC
    FIELD volumen AS DEC
    FIELD indice AS CHAR FORMAT 'x(20)'
    FIELD CHR__02 AS CHAR 
    FIELD CatConta AS CHAR
    FIELD TpoArt AS CHAR
    .
FOR EACH almmmatg NO-LOCK WHERE almmmatg.codcia = 001:
    CREATE detalle.
    ASSIGN
        detalle.codmat = almmmatg.codmat
        detalle.desmat = almmmatg.desmat
        detalle.desmar = almmmatg.desmar
        detalle.codpro = almmmatg.codpr1
        detalle.licencia = almmmatg.licencia[1]
        detalle.monvta = almmmatg.monvta
        detalle.tpocmb = almmmatg.tpocmb
        detalle.codfam = almmmatg.codfam
        detalle.subfam = almmmatg.subfam
        detalle.undstk = almmmatg.undstk
        detalle.ctolis = almmmatg.ctolis
        detalle.peso   = almmmatg.pesmat
        detalle.volumen = almmmatg.libre_d02
        detalle.indice = almmmatg.flgcomercial
        detalle.CHR__02 = almmmatg.CHR__02
        detalle.catconta = almmmatg.catconta[1]
        detalle.tpoart = almmmatg.tpoart
        .            
        IF almmmatg.monvta = 2 THEN DO:
            /* Ic - 24Jun2019, Max indico a 2 digitos */
            ASSIGN detalle.ctolis = ROUND(almmmatg.ctolis * x-tipo-cambio,2).
        END.

    /* Licencia */
    FIND Almtabla WHERE Almtabla.Tabla = 'LC' AND 
        Almtabla.Codigo = detalle.licencia NO-LOCK NO-ERROR.
    IF AVAILABLE Almtabla THEN DO:
        detalle.licencia = Almtabla.nombre.
        detalle.licenciatario = Almtabla.nomant.
        /* Licenciatario */
        FIND Almtabla WHERE Almtabla.Tabla = 'LN' AND
            Almtabla.Codigo = detalle.licenciatario NO-LOCK NO-ERROR.
        IF AVAILABLE Almtabla THEN detalle.licenciatario = almtabla.nombre.
    END.
    /* Ranking */
    FIND Factabla WHERE Factabla.codcia = 001 AND
        Factabla.Tabla = 'RANKVTA' AND
        Factabla.Codigo = Almmmatg.codmat NO-LOCK NO-ERROR.
    IF AVAILABLE Factabla THEN DO:
        /* Campaña */
        detalle.rank_gral_c = Factabla.Campo-c[1].
        detalle.rank_utilex_c = Factabla.Campo-c[2].
        detalle.rank_mayorista_c = Factabla.Campo-c[3].
        /* NO Campaña */
        detalle.rank_gral_nc = Factabla.Campo-c[4].
        detalle.rank_utilex_nc = Factabla.Campo-c[5].
        detalle.rank_mayorista_nc = Factabla.Campo-c[6].
    END.
END.
x-Archivo = "/home/v/IN/dbs/" + "almmmatg.txt".
OUTPUT TO VALUE(x-Archivo) KEEP-MESSAGES.
FOR EACH detalle NO-LOCK:
    EXPORT DELIMITER "~029" detalle.
END.
OUTPUT CLOSE.

x-Archivo = "/home/v/IN/dbs/" + "ccb_pendientes.txt".
OUTPUT TO VALUE(x-Archivo) KEEP-MESSAGES.
FOR EACH ccb_pendientes NO-LOCK:
    EXPORT DELIMITER "~029" ccb_pendientes.
END.
OUTPUT CLOSE.

/* INGRESOS A CAJA */
x-Archivo = "/home/v/IN/dbs/" + "ccbccaja" + STRING(DAY(x-CodFchI),'99') + 
    STRING(MONTH(x-CodFchI),'99') + STRING(YEAR(x-CodFchI),'9999') + ".txt".
OUTPUT TO VALUE(x-archivo) KEEP-MESSAGES.
FOR EACH ccbccaja NO-LOCK WHERE CcbCCaja.CodCia = s-codcia AND
    (CcbCCaja.CodDoc = "I/C" OR 
    (CcbCCaja.CodDoc = "E/C" AND LOOKUP(CcbCCaja.Tipo, "ANTREC,DEVONC,DEVOBD") > 0)) AND
    CcbCCaja.FchDoc >= x-CodFchI AND
    CcbCCaja.FchDoc <= x-CodFchF AND
    CcbCCaja.FlgEst <> "A":
    EXPORT DELIMITER "~029" ccbccaja EXCEPT ccbccaja.glosa.
END.
OUTPUT CLOSE.
DEF TEMP-TABLE t-ccbdcaja LIKE ccbdcaja
    FIELD fchvto AS DATE FORMAT '99/99/9999'.
FOR EACH ccbccaja NO-LOCK WHERE CcbCCaja.CodCia = s-codcia AND
    (CcbCCaja.CodDoc = "I/C" OR 
    (CcbCCaja.CodDoc = "E/C" AND LOOKUP(CcbCCaja.Tipo, "ANTREC,DEVONC,DEVOBD") > 0)) AND
    CcbCCaja.FchDoc >= x-CodFchI AND
    CcbCCaja.FchDoc <= x-CodFchF AND
    CcbCCaja.FlgEst <> "A",
    EACH CcbDCaja OF CcbCCaja NO-LOCK:
    CREATE t-ccbdcaja.
    BUFFER-COPY ccbdcaja TO t-ccbdcaja.
    FIND ccbcdocu WHERE ccbcdocu.codcia = ccbdcaja.codcia AND
        ccbcdocu.coddoc = ccbdcaja.codref AND
        ccbcdocu.nrodoc = ccbdcaja.nroref NO-LOCK NO-ERROR.
    IF AVAILABLE ccbcdocu THEN t-ccbdcaja.fchvto = ccbcdocu.fchvto.
END.
x-Archivo = "/home/v/IN/dbs/" + "ccbdcaja" + STRING(DAY(x-CodFchI),'99') +
    STRING(MONTH(x-CodFchI),'99') + STRING(YEAR(x-CodFchI),'9999') + ".txt".
OUTPUT TO VALUE(x-archivo) KEEP-MESSAGES.
FOR EACH t-ccbdcaja NO-LOCK:
    EXPORT DELIMITER "~029" t-ccbdcaja.
END.
OUTPUT CLOSE.

x-Archivo = "/home/v/IN/dbs/" + "ccbdmov" + STRING(DAY(x-CodFchI),'99') +
    STRING(MONTH(x-CodFchI),'99') + STRING(YEAR(x-CodFchI),'9999') + ".txt".
OUTPUT TO VALUE(x-archivo) KEEP-MESSAGES.
FOR EACH ccbccaja NO-LOCK WHERE CcbCCaja.CodCia = s-codcia AND
    CcbCCaja.CodDoc = "I/C" AND
    CcbCCaja.FchDoc >= x-CodFchI AND
    CcbCCaja.FchDoc <= x-CodFchF AND
    CcbCCaja.FlgEst <> "A",
    EACH CCBDMOV NO-LOCK WHERE CCBDMOV.CodCia = CcbCCaja.CodCia AND
    CCBDMOV.CodRef = CcbCCaja.CodDoc AND
    CCBDMOV.NroRef = CcbCCaja.NroDoc:
    EXPORT DELIMITER "~029" ccbdmov.
END.
OUTPUT CLOSE.
/* ******************************************************************************* */
/* NOTAS BANCARIAS */
/* ******************************************************************************* */
x-Archivo = "/home/v/IN/dbs/" + "ccbcmvto" + STRING(DAY(x-CodFchI),'99') + 
    STRING(MONTH(x-CodFchI),'99') + STRING(YEAR(x-CodFchI),'9999') + ".txt".
OUTPUT TO VALUE(x-archivo) KEEP-MESSAGES.
FOR EACH ccbcmvto NO-LOCK WHERE ccbcmvto.CodCia = s-codcia AND
    ccbcmvto.CodDoc = "N/B" AND
    ccbcmvto.FchCbd >= x-CodFchI AND
    ccbcmvto.FchCbd <= x-CodFchF AND
    ccbcmvto.FlgEst <> "A":
    EXPORT DELIMITER "~029" ccbcmvto.
END.
OUTPUT CLOSE.
/* Cargamos temporal */
EMPTY TEMP-TABLE t-ccbdcaja.
FOR EACH ccbcmvto NO-LOCK WHERE ccbcmvto.CodCia = s-codcia AND
    ccbcmvto.CodDoc = "N/B" AND
    ccbcmvto.FchCbd >= x-CodFchI AND
    ccbcmvto.FchCbd <= x-CodFchF AND
    ccbcmvto.FlgEst <> "A",
    EACH ccbdcaja NO-LOCK WHERE ccbdcaja.codcia = ccbcmvto.codcia AND
    ccbdcaja.coddoc = ccbcmvto.coddoc AND
    ccbdcaja.nrodoc = ccbcmvto.nrodoc:
    CREATE t-ccbdcaja.
    BUFFER-COPY ccbdcaja TO t-ccbdcaja.
    FIND ccbcdocu WHERE ccbcdocu.codcia = ccbdcaja.codcia AND
        ccbcdocu.coddoc = ccbdcaja.codref AND
        ccbcdocu.nrodoc = ccbdcaja.nroref NO-LOCK NO-ERROR.
    IF AVAILABLE ccbcdocu THEN t-ccbdcaja.fchvto = ccbcdocu.fchvto.
END.
x-Archivo = "/home/v/IN/dbs/" + "ccbdmvto" + STRING(DAY(x-CodFchI),'99') +
    STRING(MONTH(x-CodFchI),'99') + STRING(YEAR(x-CodFchI),'9999') + ".txt".
OUTPUT TO VALUE(x-archivo) KEEP-MESSAGES.
FOR EACH t-ccbdcaja NO-LOCK:
    EXPORT DELIMITER "~029" t-ccbdcaja.
END.
OUTPUT CLOSE.

/* COMPROBANTES */
DEF VAR x-TpoCmbCmp LIKE Gn-Tcmb.Compra NO-UNDO.
DEF VAR x-TpoCmbVta LIKE Gn-Tcmb.Venta  NO-UNDO.

DEF TEMP-TABLE t-ccbcdocu
    FIELD CodDiv like CcbCDocu.CodDiv
    FIELD CodDoc like CcbCDocu.CodDoc 
    FIELD NroDoc like CcbCDocu.NroDoc 
    FIELD FchDoc like CcbCDocu.FchDoc 
    FIELD FchVto like CcbCDocu.FchVto 
    FIELD CodCli like CcbCDocu.CodCli 
    FIELD ImpTot like CcbCDocu.ImpTot 
    FIELD SdoAct like CcbCDocu.SdoAct 
    FIELD FlgEst like CcbCDocu.FlgEst 
    FIELD CodMon like CcbCDocu.CodMon 
    FIELD TpoCmbVta LIKE x-TpoCmbVta
    FIELD FmaPgo LIKE CcbCDocu.FmaPgo 
    FIELD DivOri LIKE CcbCDocu.DivOri
    FIELD Flete  AS DECIMAL 
    FIELD CodVen LIKE CcbCDocu.CodVen
    FIELD CodRef LIKE ccbcdocu.codref
    FIELD nroref LIKE ccbcdocu.nroref
    .

FOR EACH FacDocum NO-LOCK WHERE FacDocum.CodCia = s-CodCia AND FacDocum.TpoDoc = YES,
    EACH Ccbcdocu NO-LOCK WHERE Ccbcdocu.codcia = s-CodCia AND
    Ccbcdocu.coddoc = FacDocum.CodDoc AND
    Ccbcdocu.fchdoc >= x-CodFchI AND
    Ccbcdocu.fchdoc <= x-CodFchF:
    x-TpoCmbVta = Ccbcdocu.TpoCmb.
    FIND LAST Gn-Tcmb WHERE Gn-Tcmb.Fecha <= CcbCdocu.FchDoc NO-LOCK NO-ERROR.
    IF NOT AVAIL Gn-Tcmb THEN 
        FIND FIRST Gn-Tcmb WHERE Gn-Tcmb.Fecha >= CcbCdocu.FchDoc NO-LOCK NO-ERROR.
    IF AVAIL Gn-Tcmb THEN 
        ASSIGN
            x-TpoCmbCmp = Gn-Tcmb.Compra
            x-TpoCmbVta = Gn-Tcmb.Venta.
    CREATE t-ccbcdocu.
    BUFFER-COPY CcbCDocu TO t-ccbcdocu
        ASSIGN
        t-ccbcdocu.TpoCmbVta = x-TpoCmbVta.
    IF LOOKUP(Ccbcdocu.coddoc, 'FAC,BOL') > 0 THEN DO:
        FIND gn-divi WHERE gn-divi.codcia = Ccbcdocu.codcia AND
            gn-divi.coddiv = Ccbcdocu.divori NO-LOCK NO-ERROR.
        IF AVAILABLE gn-divi AND gn-divi.CanalVenta <> "MIN" THEN DO:
            FOR EACH Ccbddocu OF Ccbcdocu NO-LOCK:
                t-ccbcdocu.flete = t-ccbcdocu.flete + (ccbddocu.candes * ccbddocu.impdcto_adelanto[4]).
            END.
        END.
    END.
END.
x-Archivo = "/home/v/IN/dbs/" + "ccbcdocu" + STRING(DAY(x-CodFchI),'99') +
    STRING(MONTH(x-CodFchI),'99') + STRING(YEAR(x-CodFchI),'9999') + ".txt".
OUTPUT TO VALUE(x-archivo) KEEP-MESSAGES.
FOR EACH t-ccbcdocu NO-LOCK:
    EXPORT DELIMITER "~029" t-ccbcdocu.
END.
/* FOR EACH FacDocum NO-LOCK WHERE FacDocum.CodCia = s-CodCia AND FacDocum.TpoDoc = YES, */
/*     EACH Ccbcdocu NO-LOCK WHERE Ccbcdocu.codcia = s-CodCia AND                        */
/*     Ccbcdocu.coddoc = FacDocum.CodDoc AND                                             */
/*     Ccbcdocu.fchdoc >= x-CodFchI AND                                                  */
/*     Ccbcdocu.fchdoc <= x-CodFchF:                                                     */
/*     x-TpoCmbVta = Ccbcdocu.TpoCmb.                                                    */
/*     FIND LAST Gn-Tcmb WHERE Gn-Tcmb.Fecha <= CcbCdocu.FchDoc NO-LOCK NO-ERROR.        */
/*     IF NOT AVAIL Gn-Tcmb THEN                                                         */
/*         FIND FIRST Gn-Tcmb WHERE Gn-Tcmb.Fecha >= CcbCdocu.FchDoc NO-LOCK NO-ERROR.   */
/*     IF AVAIL Gn-Tcmb THEN                                                             */
/*         ASSIGN                                                                        */
/*             x-TpoCmbCmp = Gn-Tcmb.Compra                                              */
/*             x-TpoCmbVta = Gn-Tcmb.Venta.                                              */
/*     EXPORT DELIMITER "~029"                                                           */
/*         CcbCDocu.CodDiv                                                               */
/*         CcbCDocu.CodDoc                                                               */
/*         CcbCDocu.NroDoc                                                               */
/*         CcbCDocu.FchDoc                                                               */
/*         CcbCDocu.FchVto                                                               */
/*         CcbCDocu.CodCli                                                               */
/*         /*CcbCDocu.NomCli */                                                          */
/*         CcbCDocu.ImpTot                                                               */
/*         CcbCDocu.SdoAct                                                               */
/*         CcbCDocu.FlgEst                                                               */
/*         CcbCDocu.CodMon                                                               */
/*         x-TpoCmbVta /*CcbCDocu.TpoCmb */                                              */
/*         CcbCDocu.FmaPgo                                                               */
/*         CcbCDocu.DivOri                                                               */
/*                                                                                       */
/*         .                                                                             */
/* END.                                                                                  */
OUTPUT CLOSE.

/* ******************************************************************************* */
/* CANJE RENOVACION y REFINANCIACION DE LETRAS */
/* ******************************************************************************* */
x-Archivo = "/home/v/IN/dbs/" + "ccbcmvto" + STRING(DAY(x-CodFchI),'99') + 
    STRING(MONTH(x-CodFchI),'99') + STRING(YEAR(x-CodFchI),'9999') + ".txt".
OUTPUT TO VALUE(x-archivo) APPEND KEEP-MESSAGES.
FOR EACH ccbcmvto NO-LOCK WHERE ccbcmvto.CodCia = s-codcia AND
    LOOKUP(ccbcmvto.CodDoc, "CJE,REF,RNV") > 0 AND
    ccbcmvto.FchDoc >= x-CodFchI AND
    ccbcmvto.FchDoc <= x-CodFchF AND
    ccbcmvto.FlgEst = "E":
    EXPORT DELIMITER "~029" ccbcmvto.
END.
OUTPUT CLOSE.
/* Cargamos temporal */
EMPTY TEMP-TABLE t-ccbdcaja.
FOR EACH ccbcmvto NO-LOCK WHERE ccbcmvto.CodCia = s-codcia AND
    LOOKUP(ccbcmvto.CodDoc, "CJE,REF,RNV") > 0 AND
    ccbcmvto.FchDoc >= x-CodFchI AND
    ccbcmvto.FchDoc <= x-CodFchF AND
    ccbcmvto.FlgEst = "E",
    EACH ccbdcaja NO-LOCK WHERE ccbdcaja.codcia = ccbcmvto.codcia AND
    ccbdcaja.coddoc = ccbcmvto.coddoc AND
    ccbdcaja.nrodoc = ccbcmvto.nrodoc:
    CREATE t-ccbdcaja.
    BUFFER-COPY ccbdcaja TO t-ccbdcaja.
    FIND ccbcdocu WHERE ccbcdocu.codcia = ccbdcaja.codcia AND
        ccbcdocu.coddoc = ccbdcaja.codref AND
        ccbcdocu.nrodoc = ccbdcaja.nroref NO-LOCK NO-ERROR.
    IF AVAILABLE ccbcdocu THEN t-ccbdcaja.fchvto = ccbcdocu.fchvto.
END.
x-Archivo = "/home/v/IN/dbs/" + "ccbdmvto" + STRING(DAY(x-CodFchI),'99') +
    STRING(MONTH(x-CodFchI),'99') + STRING(YEAR(x-CodFchI),'9999') + ".txt".
OUTPUT TO VALUE(x-archivo) APPEND KEEP-MESSAGES.
FOR EACH t-ccbdcaja NO-LOCK:
    EXPORT DELIMITER "~029" t-ccbdcaja.
END.
OUTPUT CLOSE.

RUN Rutas.

RUN Articulos.

DEFINE VARIABLE comm-line AS CHARACTER FORMAT "x(70)".                                        
comm-line = "/usr/bin/qonvtaexport2".
OS-COMMAND VALUE(comm-line) SILENT NO-CONSOLE.
PUT UNFORMATTED 'PROCESO TERMINADO ' NOW SKIP.

QUIT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-Articulos) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Articulos Procedure 
PROCEDURE Articulos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/* ARTICULOS */
x-Archivo = "/home/v/IN/dbs/" + "articulos" + STRING(DAY(x-CodFchI),'99') +
    STRING(MONTH(x-CodFchI),'99') + STRING(YEAR(x-CodFchI),'9999') + ".txt".
OUTPUT TO VALUE(x-archivo) APPEND KEEP-MESSAGES.
FOR EACH Almmmatg NO-LOCK WHERE Almmmatg.codcia = s-codcia:
    EXPORT DELIMITER "~029" Almmmatg.codmat Almmmatg.desmat Almmmatg.catconta[1] 
        Almmmatg.codfam Almmmatg.subfam Almmmatg.undbas Almmmatg.codmar.
END.
OUTPUT CLOSE.
/* BARRAS */
DEF VAR k AS INT NO-UNDO.
x-Archivo = "/home/v/IN/dbs/" + "barras" + STRING(DAY(x-CodFchI),'99') +
    STRING(MONTH(x-CodFchI),'99') + STRING(YEAR(x-CodFchI),'9999') + ".txt".
OUTPUT TO VALUE(x-archivo).
FOR EACH Almmmatg NO-LOCK WHERE Almmmatg.codcia = s-codcia:
    IF Almmmatg.CodBrr > '' THEN EXPORT DELIMITER "~029" Almmmatg.codmat "EAN13" Almmmatg.CodBrr 1.
    /* EAN 14 */
    FOR EACH Almmmat1 OF Almmmatg NO-LOCK:
        DO k = 1 TO 5:
            IF Almmmat1.Barras[k] > '' THEN
                EXPORT DELIMITER "~029" Almmmatg.codmat "EAN14" Almmmat1.Barras[k] Almmmat1.Equival[k].
        END.
    END.
END.
OUTPUT CLOSE.
/* MARCAS */
x-Archivo = "/home/v/IN/dbs/" + "marcas" + STRING(DAY(x-CodFchI),'99') +
    STRING(MONTH(x-CodFchI),'99') + STRING(YEAR(x-CodFchI),'9999') + ".txt".
OUTPUT TO VALUE(x-archivo).
FOR EACH Almtabla NO-LOCK WHERE Almtabla.Tabla = "MK":
    EXPORT DELIMITER "~029" Almtabla.Codigo Almtabla.Nombre.
END.
OUTPUT CLOSE.
/* FAMILIAS */
x-Archivo = "/home/v/IN/dbs/" + "familias" + STRING(DAY(x-CodFchI),'99') +
    STRING(MONTH(x-CodFchI),'99') + STRING(YEAR(x-CodFchI),'9999') + ".txt".
OUTPUT TO VALUE(x-archivo).
FOR EACH Almtfami NO-LOCK WHERE Almtfami.codcia = s-codcia:
    EXPORT DELIMITER "~029" Almtfami.codfam Almtfami.desfam.
END.
OUTPUT CLOSE.
/* SUB-FAMILIAS */
x-Archivo = "/home/v/IN/dbs/" + "subfamilias" + STRING(DAY(x-CodFchI),'99') +
    STRING(MONTH(x-CodFchI),'99') + STRING(YEAR(x-CodFchI),'9999') + ".txt".
OUTPUT TO VALUE(x-archivo).
FOR EACH Almsfami NO-LOCK WHERE Almsfami.codcia = s-codcia:
    EXPORT DELIMITER "~029" AlmSFami.subfam AlmSFami.dessub AlmSFami.codfam.
END.
OUTPUT CLOSE.
/* UNIDADES */
x-Archivo = "/home/v/IN/dbs/" + "unidades" + STRING(DAY(x-CodFchI),'99') +
    STRING(MONTH(x-CodFchI),'99') + STRING(YEAR(x-CodFchI),'9999') + ".txt".
OUTPUT TO VALUE(x-archivo).
FOR EACH Unidades NO-LOCK:
    EXPORT DELIMITER "~029" Unidades.Codunid Unidades.Desunid.
END.
OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-clientes) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE clientes Procedure 
PROCEDURE clientes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/* ******************************************************************************** */
/* CLIENTES */
/* ******************************************************************************** */
EMPTY TEMP-TABLE t-cliente.
FOR EACH gn-clie NO-LOCK WHERE gn-clie.codcia = 0:  /* AND gn-clie.Fching >= (TODAY - 1): */
    CREATE t-cliente.
    BUFFER-COPY gn-clie TO t-cliente.
    FIND VtaTabla WHERE VtaTabla.CodCia = 001 AND 
        VtaTabla.Tabla = 'CLNOPER' AND
        VtaTabla.Llave_c1 = gn-clie.ruc
        NO-LOCK NO-ERROR.
    IF AVAILABLE VtaTabla THEN t-cliente.exceptua = YES.
    ASSIGN
        t-cliente.e-maile = gn-clie.transporte[4].
END.
x-Archivo = "/home/v/IN/dbs/" + "gn-clie.txt".
OUTPUT TO VALUE(x-archivo) KEEP-MESSAGES.
FOR EACH t-cliente NO-LOCK :
    EXPORT delimiter "~029" t-cliente.
END.
OUTPUT CLOSE.
/* ******************************************************************************** */
/* SEDE DE LOS CLIENTES */
/* ******************************************************************************** */
EMPTY TEMP-TABLE t-gn-clied.
FOR EACH gn-clied NO-LOCK WHERE gn-clied.codcia = 0:
    CREATE t-gn-clied.
    BUFFER-COPY gn-clied TO t-gn-clied.
END.
x-Archivo = "/home/v/IN/dbs/" + "gn-clied.txt".
OUTPUT TO VALUE(x-archivo) KEEP-MESSAGES.
FOR EACH t-gn-clied NO-LOCK :
    EXPORT delimiter "~029" t-gn-clied.
END.
OUTPUT CLOSE.
/* ******************************************************************************** */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Rutas) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Rutas Procedure 
PROCEDURE Rutas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


/* DI-RUTAC */
x-Archivo = "/home/v/IN/dbs/" + "di-rutac" + STRING(DAY(x-CodFchI),'99') + 
    STRING(MONTH(x-CodFchI),'99') + STRING(YEAR(x-CodFchI),'9999') + ".txt".
OUTPUT TO VALUE(x-archivo) KEEP-MESSAGES.
FOR EACH DI-RutaC NO-LOCK WHERE DI-RutaC.CodCia = s-CodCia AND
    (DI-RutaC.CodDoc = "PHR" OR DI-RutaC.CodDoc = "H/R") AND
    DI-RutaC.FchDoc >= x-CodFchI AND
    DI-RutaC.FchDoc <= x-CodFchF:
    EXPORT DELIMITER "~029" DI-RutaC.
END.
OUTPUT CLOSE.
/* DI-RUTAD */
x-Archivo = "/home/v/IN/dbs/" + "di-rutad" + STRING(DAY(x-CodFchI),'99') + 
    STRING(MONTH(x-CodFchI),'99') + STRING(YEAR(x-CodFchI),'9999') + ".txt".
OUTPUT TO VALUE(x-archivo) KEEP-MESSAGES.
FOR EACH DI-RutaC NO-LOCK WHERE DI-RutaC.CodCia = s-CodCia AND
    (DI-RutaC.CodDoc = "PHR" OR DI-RutaC.CodDoc = "H/R") AND
    DI-RutaC.FchDoc >= x-CodFchI AND
    DI-RutaC.FchDoc <= x-CodFchF,
    EACH DI-RutaD OF DI-RutaC NO-LOCK:
    EXPORT DELIMITER "~029" DI-RutaD.
END.
OUTPUT CLOSE.
/* DI-RUTADG */
x-Archivo = "/home/v/IN/dbs/" + "di-rutadg" + STRING(DAY(x-CodFchI),'99') + 
    STRING(MONTH(x-CodFchI),'99') + STRING(YEAR(x-CodFchI),'9999') + ".txt".
OUTPUT TO VALUE(x-archivo) KEEP-MESSAGES.
FOR EACH DI-RutaC NO-LOCK WHERE DI-RutaC.CodCia = s-CodCia AND
    DI-RutaC.CodDoc = "H/R" AND
    DI-RutaC.FchDoc >= x-CodFchI AND
    DI-RutaC.FchDoc <= x-CodFchF,
    EACH DI-RutaDG OF DI-RutaC NO-LOCK:
    EXPORT DELIMITER "~029" DI-RutaDG.
END.
OUTPUT CLOSE.
/* DI-RUTAG */
x-Archivo = "/home/v/IN/dbs/" + "di-rutag" + STRING(DAY(x-CodFchI),'99') + 
    STRING(MONTH(x-CodFchI),'99') + STRING(YEAR(x-CodFchI),'9999') + ".txt".
OUTPUT TO VALUE(x-archivo) KEEP-MESSAGES.
FOR EACH DI-RutaC NO-LOCK WHERE DI-RutaC.CodCia = s-CodCia AND
    DI-RutaC.CodDoc = "H/R" AND
    DI-RutaC.FchDoc >= x-CodFchI AND
    DI-RutaC.FchDoc <= x-CodFchF,
    EACH DI-RutaG OF DI-RutaC NO-LOCK:
    EXPORT DELIMITER "~029" DI-RutaG.
END.
OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

