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
         HEIGHT             = 5.5
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
DEF NEW SHARED VAR s-codcia AS INTE INIT 001.
SESSION:DATE-FORMAT = 'ymd'.    /* Pedido por B.Acuña */

DEF TEMP-TABLE Resumen
    FIELD codalm AS CHAR FORMAT 'x(8)' LABEL 'Almacen'
    FIELD codmat AS CHAR FORMAT 'x(10)' LABEL 'Articulo'
    FIELD stkact AS DECI FORMAT '->>>,>>>,>>9.9999' LABEL 'Stock'
    FIELD ctopro AS DECI FORMAT '->>>,>>>,>>9.9999' LABEL 'Cto Unitario'
    FIELD stockmax AS DECI FORMAT '->>>,>>>,>>9.9999' 
    FIELD reservado AS DECI FORMAT '->>>,>>>,>>9.9999'
    FIELD trftransito AS DECI FORMAT '->>>,>>>,>>9.9999' 
    FIELD cmptransito AS DECI FORMAT '->>>,>>>,>>9.9999' 
    FIELD vctmn1 AS DECI FORMAT '->>>,>>>,>>9.9999' 
    FIELD vctmn2 AS DECI FORMAT '->>>,>>>,>>9.9999' 
    FIELD stkrep AS DECI FORMAT '->>>,>>>,>>9.9999' 
    FIELD canemp AS DECI FORMAT '->>>,>>>,>>9.9999' 
    FIELD fchultmov AS DATE FORMAT '99/99/9999'
    INDEX Idx00 AS PRIMARY codalm codmat
    .

DEF VAR x-StockComprometido AS DECI NO-UNDO.

/* 17/10/23: Todo */
PUT 'INICIO: ' NOW SKIP.
FOR EACH almacen NO-LOCK WHERE almacen.codcia = s-codcia,
    EACH almmmate OF almacen NO-LOCK WHERE almmmate.stkact <> 0,
    FIRST almmmatg OF almmmate NO-LOCK:
    CREATE Resumen.
    ASSIGN
        Resumen.codalm = Almmmate.codalm
        Resumen.codmat = Almmmate.codmat
        Resumen.stkact = Almmmate.stkact
        Resumen.stockmax = Almmmate.StockMax
        .
    FIND LAST AlmStkge USE-INDEX Llave01 WHERE AlmStkge.CodCia = s-codcia AND
        AlmStkge.codmat = Almmmate.codmat AND
        AlmStkge.Fecha <= TODAY
        NO-LOCK NO-ERROR.
    IF AVAILABLE AlmStkge THEN Resumen.ctopro = AlmStkge.CtoUni.
    FIND LAST AlmStkal USE-INDEX Llave01 WHERE AlmStkal.CodCia = s-codcia AND
        AlmStkal.codalm = Almmmate.codalm AND 
        AlmStkal.codmat = Almmmate.codmat AND
        AlmStkal.Fecha <= TODAY
        NO-LOCK NO-ERROR.
    IF AVAILABLE AlmStkal THEN Resumen.FchUltMov = AlmStkal.Fecha.

    /* Stock comprometido */
    x-StockComprometido = 0.
    RUN ./gn/stock-comprometido-v2.p (Resumen.codmat, 
                                      Resumen.codalm, 
                                      NO,
                                      OUTPUT x-StockComprometido).
    ASSIGN
        Resumen.reservado = x-StockComprometido.
    ASSIGN
        Resumen.vctmn1 = Almmmate.vctmn1
        Resumen.vctmn2 = Almmmate.vctmn2
        Resumen.stkrep = Almmmatg.stkrep
        Resumen.canemp = Almmmatg.canemp
        .
END.
/* Transferencias por recepcionar */
PUT 'TRANSITO: ' NOW SKIP.
RUN Carga-Transferencias.

/* Ordenes de Compra en Transito */
PUT 'COMPRAS: ' NOW SKIP.
RUN Carga-Compras.

/* ********************************************************************************* */
/* Pasamos el archivo texto */
/* ********************************************************************************* */
PUT 'TEXTO: ' NOW SKIP.
DEF VAR x-Archivo AS CHAR NO-UNDO.

x-Archivo = "/home/v/IN/dbs/" + "stockdepostock.txt".
OUTPUT TO VALUE(x-archivo) KEEP-MESSAGES.
FOR EACH Resumen NO-LOCK:
    EXPORT DELIMITER ";" Resumen.
END.
OUTPUT CLOSE.
/* ******************************************************************************* */

/* ******************************************************************************* */
/* EXPORTA A OLAP */
/* ******************************************************************************* */
DEFINE VARIABLE comm-line AS CHARACTER FORMAT "x(70)".                                        

/*comm-line = "scp -i id_rsa /home/v/IN/dbs/stockdepostock.txt root@192.168.100.171:/home/data".*/
PUT 'EXPORT TXT: ' NOW SKIP.

comm-line = "/usr/bin/qonvtaexport7".
OS-COMMAND VALUE(comm-line) SILENT NO-CONSOLE.

/*
curl -X POST http://192.168.0.231:5000/datawarehouse/stockdepo/olap/stock/artcode 
-H 'accept:application/json' 
-H 'Content-Type:multipart/form-data' 
-F 'file=@stockdepostock.txt;type=text/plain'
*/

PUT 'FIN: ' NOW SKIP.

QUIT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-Carga-Compras) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Carga-Compras Procedure 
PROCEDURE Carga-Compras :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    FOR EACH OOComPend NO-LOCK WHERE (OOComPend.CanPed - OOComPend.CanAte) > 0:
        FIND FIRST Resumen WHERE Resumen.codalm = OOComPend.CodAlm
            AND Resumen.codmat = OOComPend.CodMat
            EXCLUSIVE-LOCK NO-ERROR.
        IF AVAILABLE Resumen THEN 
            ASSIGN Resumen.CmpTransito = Resumen.CmpTransito + (OOComPend.CanPed - OOComPend.CanAte).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Carga-Transferencias) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Carga-Transferencias Procedure 
PROCEDURE Carga-Transferencias :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/* REPOSICIONES */
FOR EACH Almacen NO-LOCK WHERE Almacen.codcia = s-codcia
        AND LOOKUP(Almacen.CodAlm, '997,998') = 0,
    EACH Almcrepo NO-LOCK WHERE Almcrepo.codcia = Almacen.codcia
        AND Almcrepo.CodAlm = Almacen.CodAlm
        AND Almcrepo.FlgEst = 'P',
    EACH Almdrepo OF Almcrepo NO-LOCK WHERE almdrepo.CanApro > almdrepo.CanAten:
    FIND FIRST Resumen WHERE Resumen.codalm = Almcrepo.CodAlm
        AND Resumen.codmat = Almdrepo.CodMat
        EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE Resumen THEN 
        ASSIGN 
        Resumen.TrfTransito = Resumen.TrfTransito + (Almdrepo.CanApro - Almdrepo.CanAten).
END.
/* TRANSFERENCIAS */
DEF VAR cAlmDes AS CHAR NO-UNDO.
FOR EACH Almacen NO-LOCK WHERE Almacen.codcia = s-codcia,
    EACH Almcmov NO-LOCK WHERE Almcmov.codcia = Almacen.codcia
        AND Almcmov.codalm = Almacen.codalm
        AND Almcmov.tipmov = "S"
        AND Almcmov.codmov = 03
        AND Almcmov.flgest <> "A"
        AND Almcmov.flgsit = "T",
    EACH Almdmov OF Almcmov NO-LOCK:
    /* RHC 05/02/2018 Dos casos: 
    1. Salida con Cross Docking
    2. Salida sin Cross Docking 
    */
    IF Almcmov.CrossDocking = YES THEN cAlmDes = Almcmov.AlmacenXD.     /* Destino Final */
    ELSE cAlmDes = Almcmov.AlmDes.  /* Almacén Destino */
    FIND FIRST Resumen WHERE Resumen.codalm = cAlmDes
        AND Resumen.codmat = Almdmov.CodMat
        EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE Resumen THEN 
        ASSIGN 
        Resumen.TrfTransito = Resumen.TrfTransito + Almdmov.candes.
END.
/* ORDENES DE TRANSFERENCIA */
FOR EACH gn-divi NO-LOCK WHERE GN-DIVI.CodCia = s-codcia,
    EACH FacCPedi NO-LOCK WHERE FacCPedi.CodCia = GN-DIVI.CodCia
        AND FacCPedi.CodDiv = GN-DIVI.CodDiv
        AND FacCPedi.CodDoc = "OTR"
        AND FacCPedi.FlgEst = "P"
        AND FacCPedi.CodRef = "R/A",
    EACH Facdpedi OF FacCPedi NO-LOCK WHERE Facdpedi.flgest = 'P':
    /* RHC 05/02/2018 Dos casos: 
    1. Salida con Cross Docking
    2. Salida sin Cross Docking 
    */
    IF FacCPedi.CrossDocking = YES THEN cAlmDes = FacCPedi.AlmacenXD.     /* Destino Final */
    ELSE cAlmDes = FacCPedi.CodCli.  /* Almacén Destino */
    FIND FIRST Resumen WHERE Resumen.codalm = cAlmDes
        AND Resumen.codmat = Facdpedi.CodMat
        EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE Resumen THEN 
        ASSIGN
        Resumen.TrfTransito = Resumen.TrfTransito + Facdpedi.Factor * (Facdpedi.CanPed - Facdpedi.CanAte).
END.
/* ********************************************************************************************************* */
/* RHC 16/06/2021 Sloting */
/* ********************************************************************************************************* */
FOR EACH OOMoviAlmacen NO-LOCK WHERE OOMoviAlmacen.codcia = s-codcia
    AND OOMoviAlmacen.FlagMigracion = "N" 
    AND OOMoviAlmacen.TipMov = "I" 
    AND OOMoviAlmacen.CodMov = 03:
    FIND FIRST Resumen WHERE Resumen.codalm = OOMoviAlmacen.CodAlm
        AND Resumen.codmat = OOMoviAlmacen.CodMat
        EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE Resumen THEN 
        ASSIGN 
        Resumen.TrfTransito = Resumen.TrfTransito + (OOMoviAlmacen.CanDes * OOMoviAlmacen.Factor).
END.
FOR EACH OOMoviAlmacen NO-LOCK WHERE OOMoviAlmacen.codcia = s-codcia
    AND OOMoviAlmacen.FlagMigracion = "N" 
    AND OOMoviAlmacen.FchDoc >= DATE(06,01,2019) 
    AND OOMoviAlmacen.TipMov = "I" 
    AND OOMoviAlmacen.CodMov = 90
    AND OOMoviAlmacen.UseInDropShipment = "NO":
    FIND FIRST Resumen WHERE Resumen.codalm = OOMoviAlmacen.CodAlm
        AND Resumen.codmat = OOMoviAlmacen.CodMat
        EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE Resumen THEN 
        ASSIGN 
        Resumen.TrfTransito = Resumen.TrfTransito + (OOMoviAlmacen.CanDes * OOMoviAlmacen.Factor).
END.
FOR EACH OOMoviAlmacen NO-LOCK WHERE OOMoviAlmacen.codcia = s-codcia
    AND OOMoviAlmacen.FlagMigracion = "N" 
    AND OOMoviAlmacen.TipMov = "I" 
    AND (OOMoviAlmacen.CodMov = 09 OR OOMoviAlmacen.CodMov = 30)
    AND OOMoviAlmacen.UseInDropShipment = "NO":
    FIND FIRST Resumen WHERE Resumen.codalm = OOMoviAlmacen.CodAlm
        AND Resumen.codmat = OOMoviAlmacen.CodMat
        EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE Resumen THEN 
        ASSIGN 
        Resumen.TrfTransito = Resumen.TrfTransito + (OOMoviAlmacen.CanDes * OOMoviAlmacen.Factor).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

