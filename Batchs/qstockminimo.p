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
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR s-coddiv AS CHAR.
DEF VAR pv-codcia AS INT INIT 000 NO-UNDO.

DEF TEMP-TABLE Detalle
    FIELD FchDoc AS DATE
    FIELD CanDes AS DEC
    INDEX Llave01 AS PRIMARY FchDoc.

DEF VAR pRowid AS ROWID.
DEF VAR pDiasUtiles AS INT.
DEF VAR pVentaDiaria AS DEC.
DEF VAR pDiasMinimo AS INT.
DEF VAR pReposicion AS DEC.
DEF VAR pComprometido AS DEC.

DEF VAR x-StockMinimo AS DEC NO-UNDO.
DEF VAR x-StockDisponible AS DEC NO-UNDO.
DEF VAR x-Item AS INT INIT 1 NO-UNDO.
DEF VAR x-StkAct LIKE Almmmate.StkAct NO-UNDO.
DEF VAR k AS INT NO-UNDO.

/* Buscamos los valores generales */
FIND FIRST AlmCfgGn WHERE almcfggn.codcia = s-codcia NO-LOCK NO-ERROR.
IF NOT AVAILABLE Almcfggn THEN DO:
    DISPLAY 'Debe configurar los parámetros generales' WITH STREAM-IO NO-BOX NO-LABELS.
    PAUSE 0.
    RETURN.
END.
ASSIGN
    pDiasMinimo = AlmCfgGn.DiasMinimo
    pDiasUtiles = AlmCfgGn.DiasUtiles.

FOR EACH Almacen NO-LOCK WHERE Almacen.codcia = s-codcia AND Almacen.AutMov = YES:
    FOR EACH almmmate NO-LOCK WHERE Almmmate.codcia = s-codcia 
        AND Almmmate.codalm = Almacen.codalm,
        FIRST almmmatg OF almmmate NO-LOCK WHERE Almmmatg.TpoArt <> 'D':
        /* ******* */
        DISPLAY 'Procesando ' Almmmate.codalm Almmmate.codmat WITH STREAM-IO NO-BOX NO-LABELS.
        PAUSE 0.

        /* Venta Diaria */
        pRowid = ROWID(Almmmate).
        RUN venta-diaria (pRowid, pDiasUtiles, Almacen.CodAlm, OUTPUT pVentaDiaria).

        /* Stock Minimo */
        x-StockMinimo = pDiasMinimo * pVentaDiaria.
        x-StkAct = Almmmate.StkAct.
        IF x-StkAct >= x-StockMinimo THEN NEXT.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-venta-diaria) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE venta-diaria Procedure 
PROCEDURE venta-diaria :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER pRowid AS ROWID.
DEF INPUT PARAMETER pDiasUtiles AS INT.
DEF INPUT PARAMETER pAlmacenes AS CHAR.
DEF OUTPUT PARAMETER pVentaDiaria AS DEC.

FIND Almmmate WHERE ROWID(Almmmate) = pRowid
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE Almmmate THEN RETURN.

FIND Almmmatg OF Almmmate NO-LOCK NO-ERROR.
IF NOT AVAILABLE Almmmatg THEN RETURN.

/* determinamos las fechas de venta */
DEF VAR x-FchIni-Ant AS DATE NO-UNDO.
DEF VAR x-FchFin-Ant AS DATE NO-UNDO.
DEF VAR x-FchIni-Hoy AS DATE NO-UNDO.
DEF VAR x-FchFin-Hoy AS DATE NO-UNDO.
DEF VAR x-Venta-Ant AS DEC NO-UNDO.
DEF VAR x-Venta-Hoy AS DEC NO-UNDO.
DEF VAR x-Venta-Mes AS DEC NO-UNDO.

DEF VAR x-Mes AS INT NO-UNDO.
DEF VAR x-Ano AS INT NO-UNDO.
DEF VAR x-Meses AS INT INIT 3 NO-UNDO.      /* Meses de estudio estadístico */

/* 21.08.09 ahora hay que retroceder 36 dias */
x-FchFin-Ant = TODAY - 365.
x-FchIni-Ant = x-FchFin-Ant - x-Meses * 30.

RUN Venta-Diaria-Promedio-Estadistica (x-FchIni-Ant, x-FchFin-Ant, OUTPUT x-Venta-Ant).

/* 21.08.09 ahora hay que retroceder 36 dias */
x-FchFin-Hoy = TODAY.
x-FchIni-Hoy = x-FchFin-Hoy - x-Meses * 30.

RUN Venta-Diaria-Promedio-Estadistica (x-FchIni-Hoy, x-FchFin-Hoy, OUTPUT x-Venta-Hoy).

/* 21.08.09 ahora hay que retroceder 36 dias */
x-FchFin-Ant = TODAY - 365.
x-FchIni-Ant = x-FchFin-Ant - 30.

RUN Venta-Diaria-Promedio-Estadistica (x-FchIni-Ant, x-FchFin-Ant, OUTPUT x-Venta-Mes).

IF x-Venta-Ant <= 0 THEN DO:
    pVentaDiaria = x-Venta-Hoy.
    RETURN.
END.
IF ( x-Venta-Hoy / x-Venta-Ant ) > 3 OR ( x-Venta-Hoy / x-Venta-Ant ) < (1 / 3) THEN DO: 
    pVentaDiaria = x-Venta-Hoy.
    RETURN.
END.
IF ( x-Venta-Hoy / x-Venta-Mes ) > 3 OR ( x-Venta-Hoy / x-Venta-Mes ) < (1 / 3) THEN DO: 
    pVentaDiaria = x-Venta-Hoy.
    RETURN.
END.
pVentaDiaria = x-Venta-Hoy / x-Venta-Ant * x-Venta-Mes.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Venta-Diaria-Promedio-Estadistica) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Venta-Diaria-Promedio-Estadistica Procedure 
PROCEDURE Venta-Diaria-Promedio-Estadistica :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF INPUT PARAMETER pFchIni AS DATE.
DEF INPUT PARAMETER pFchFin AS DATE.
DEF OUTPUT PARAMETER pVtaPromedio AS DEC.

DEF VAR x-Items AS INT NO-UNDO.
DEF VAR x-Promedio AS DEC NO-UNDO.
DEF VAR x-Desviacion AS DEC NO-UNDO.
DEF VAR x-Fecha AS DATE NO-UNDO.

DEFINE VAR X-CODDIA AS INTEGER INIT 1 NO-UNDO.
DEFINE VAR X-CODANO AS INTEGER NO-UNDO.
DEFINE VAR X-CODMES AS INTEGER NO-UNDO.
DEFINE VAR I        AS INTEGER NO-UNDO.

/* CALCULO ESTADISTICO DE LAS VENTAS DIARIAS */
pVtaPromedio = 0.

FOR EACH Detalle:
    DELETE Detalle.
END.

FOR EACH estavtas.EvtAll01 USE-INDEX Indice01 NO-LOCK WHERE estavtas.EvtAll01.codcia = s-codcia
        AND estavtas.EvtAll01.coddiv = Almacen.coddiv
        AND estavtas.EvtAll01.codmat = Almmmate.codmat
        AND ( estavtas.EvtAll01.Nrofch >= INTEGER(STRING(YEAR(pFchIni),"9999") + STRING(MONTH(pFchIni),"99"))
        AND   estavtas.EvtAll01.Nrofch <= INTEGER(STRING(YEAR(pFchFin),"9999") + STRING(MONTH(pFchFin),"99")) ):
    /***************** Capturando el Mes siguiente *******************/
    IF estavtas.EvtAll01.Codmes < 12 THEN DO:
      ASSIGN
          X-CODMES = estavtas.EvtAll01.Codmes + 1
          X-CODANO = estavtas.EvtAll01.Codano .
    END.
    ELSE DO: 
      ASSIGN
          X-CODMES = 01
          X-CODANO = estavtas.EvtAll01.Codano + 1 .
    END.
    /*********************** Calculo Para Obtener los datos diarios ************/
     DO I = 1 TO DAY( DATE(STRING(X-CODDIA,"99") + "/" + STRING(X-CODMES,"99") + "/" + STRING(X-CODANO,"9999")) - 1 ) :        
          X-FECHA = DATE(STRING(I,"99") + "/" + STRING(estavtas.EvtAll01.Codmes,"99") + "/" + STRING(estavtas.EvtAll01.Codano,"9999")).
          IF X-FECHA >= pFchIni AND X-FECHA <= pFchFin THEN DO:
              FIND Gn-tcmb WHERE Gn-tcmb.Fecha = DATE(STRING(I,"99") + "/" + STRING(estavtas.EvtAll01.Codmes,"99") + "/" + STRING(estavtas.EvtAll01.Codano,"9999")) 
                  NO-LOCK NO-ERROR.
              IF AVAILABLE Gn-tcmb THEN DO: 
                  FIND Detalle WHERE Detalle.FchDoc = X-FECHA NO-ERROR.
                  IF NOT AVAILABLE Detalle THEN CREATE Detalle.
                  ASSIGN
                      Detalle.fchdoc = X-FECHA
                      Detalle.candes = Detalle.candes + estavtas.EvtAll01.CanxDia[I].
              END.
          END.
     END.         
END.

/* Desviacion estandar */
x-Items = 0.
x-Promedio = 0.
DO x-Fecha = pFchIni TO pFchFin:
    FIND Detalle WHERE Detalle.fchdoc = x-Fecha NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Detalle AND WEEKDAY(x-Fecha) = 1 THEN NEXT.    /* DOMINGO SIN VENTA */
    x-Items = x-Items + 1.
    IF AVAILABLE Detalle THEN x-Promedio = x-Promedio + Detalle.candes.
END.
x-Promedio = x-Promedio / x-Items.

DO x-Fecha = pFchIni TO pFchFin:
    FIND Detalle WHERE Detalle.fchdoc = x-Fecha NO-LOCK NO-ERROR.
    IF AVAILABLE Detalle 
    THEN x-Desviacion = x-Desviacion + EXP( ( Detalle.CanDes - x-Promedio ) , 2 ).
    ELSE x-Desviacion = x-Desviacion + EXP( ( 0 - x-Promedio ) , 2 ).
END.
x-Desviacion = SQRT ( x-Desviacion / ( x-Items - 1 ) ).

/* Eliminamos los items que están fuera de rango */
FOR EACH Detalle:
    IF Detalle.candes > (x-Promedio + 3 * x-Desviacion) OR Detalle.candes < (x-Promedio - 3 * x-Desviacion) 
    THEN DO:
        DELETE Detalle.
        x-Items = x-Items - 1.
    END.
END.
x-Promedio = 0.
FOR EACH Detalle:
    x-Promedio = x-Promedio + Detalle.candes.
END.
pVtaPromedio = x-Promedio / x-Items.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

