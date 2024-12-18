&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
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

DEFINE VARIABLE LocalIGV LIKE {&Cabecera}.ImpIgv NO-UNDO.
DEFINE VARIABLE LocalISC LIKE {&Cabecera}.ImpIsc NO-UNDO.
DEFINE VARIABLE LocalImpDtoAdelanto AS DECIMAL NO-UNDO.
DEFINE VARIABLE LocalIGVDtoAdelanto AS DECIMAL NO-UNDO.
DEFINE VARIABLE LocalImpLin LIKE {&Detalle}.ImpLin NO-UNDO.
DEFINE VARIABLE LocalDto2xExonerados AS DEC NO-UNDO.
DEFINE VARIABLE LocalDto2xAfectosIgv AS DEC NO-UNDO.

ASSIGN
    {&Cabecera}.ImpBrt = 0
    {&Cabecera}.ImpDto = 0
    {&Cabecera}.ImpDto2 = 0
    {&Cabecera}.ImpIgv = 0
    {&Cabecera}.ImpIsc = 0
    {&Cabecera}.ImpTot = 0
    {&Cabecera}.ImpExo = 0
    {&Cabecera}.ImpVta = 0
    {&Cabecera}.ImpTot2 = 0
    LocalIGV = 0
    LocalISC = 0
    LocalDto2xExonerados = 0
    LocalDto2xAfectosIgv = 0
    LocalImpDtoAdelanto = 0
    LocalIGVDtoAdelanto = 0
    LocalImpLin = 0
    /*{&Cabecera}.Libre_d01 = 0*/    /* DESCUENTO LISTA EXPRESS WEB */
    /*T-CDOCU.Libre_d01 = 0  /* Descuento SIN IGV por Encartes y Otros */*/
    {&Cabecera}.Libre_d02 = 0.
/* RHC 15/08/19 pedido por G.P.: Impuesto a la bolsa pl�stica */
ASSIGN
    {&Cabecera}.AcuBon[10] = 0.

FOR EACH {&Detalle} OF {&Cabecera} EXCLUSIVE-LOCK, 
    FIRST Almmmatg NO-LOCK WHERE Almmmatg.CodCia = {&Detalle}.CodCia AND Almmmatg.CodMat = {&Detalle}.CodMat:
    LocalISC = LocalISC + {&Detalle}.ImpIsc.

    {&Cabecera}.ImpTot = {&Cabecera}.ImpTot + {&Detalle}.ImpLin.
    {&Cabecera}.ImpDto2 = {&Cabecera}.ImpDto2 + {&Detalle}.ImpDto2.     /* Descuentos por Encarte o Volumen Global */

    /*
    IF {&Detalle}.AftIgv = YES THEN DO:
        LocalIGV = LocalIGV + {&Detalle}.ImpIgv.
        {&Cabecera}.ImpDto = {&Cabecera}.ImpDto + ROUND({&Detalle}.ImpDto / (1 + {&Cabecera}.PorIgv / 100), 2).
        LocalDto2xAfectosIgv = LocalDto2xAfectosIgv + {&Detalle}.ImpDto2.
        /*{&Cabecera}.Libre_d01 = {&Cabecera}.Libre_d01 + ( {&Detalle}.ImpDto2 / ( 1 + {&Cabecera}.PorIgv / 100) ).*/
    END.
    ELSE DO:
        /*{&Cabecera}.ImpExo = {&Cabecera}.ImpExo + {&Detalle}.ImpLin.*/
        {&Cabecera}.ImpDto = {&Cabecera}.ImpDto + {&Detalle}.ImpDto.
        LocalDto2xExonerados = LocalDto2xExonerados + {&Detalle}.ImpDto2.
        /*{&Cabecera}.Libre_d01 = {&Cabecera}.Libre_d01 + {&Detalle}.ImpDto2.*/
    END.
    */

    /* BOLSAS PLASTICAS */
    /* S/0.10 por cada bolsa */
    /*
    IF Almmmatg.CodFam = '086' AND Almmmatg.SubFam = '001' 
        /*     IF LOOKUP({&Detalle}.CodMat, '098878,098879,098880,098881,098882,098883') > 0 */
        THEN {&Cabecera}.AcuBon[10] = {&Cabecera}.AcuBon[10] + ({&Detalle}.CanDes * {&Detalle}.Factor * 0.10).
    */

    IF {&Detalle}.codmat = x-articulo-ICBPER THEN DO:
        {&Cabecera}.AcuBon[10] = {&Cabecera}.AcuBon[10] + {&Detalle}.implin.
    END.
    ELSE DO:
        IF NOT {&Detalle}.aftIgv THEN {&Cabecera}.ImpExo = {&Cabecera}.ImpExo + {&Detalle}.ImpLin.

        IF {&Detalle}.AftIgv = YES THEN DO:
            LocalIGV = LocalIGV + {&Detalle}.ImpIgv.
            {&Cabecera}.ImpDto = {&Cabecera}.ImpDto + ROUND({&Detalle}.ImpDto / (1 + {&Cabecera}.PorIgv / 100), 2).
            LocalDto2xAfectosIgv = LocalDto2xAfectosIgv + {&Detalle}.ImpDto2.
            /*{&Cabecera}.Libre_d01 = {&Cabecera}.Libre_d01 + ( {&Detalle}.ImpDto2 / ( 1 + {&Cabecera}.PorIgv / 100) ).*/
        END.
        ELSE DO:
            /*{&Cabecera}.ImpExo = {&Cabecera}.ImpExo + {&Detalle}.ImpLin.*/
            {&Cabecera}.ImpDto = {&Cabecera}.ImpDto + {&Detalle}.ImpDto.
            LocalDto2xExonerados = LocalDto2xExonerados + {&Detalle}.ImpDto2.
            /*{&Cabecera}.Libre_d01 = {&Cabecera}.Libre_d01 + {&Detalle}.ImpDto2.*/
        END.

    END.
END.

ASSIGN
    {&Cabecera}.ImpIgv = ROUND(LocalIGV,2)
    {&Cabecera}.ImpIsc = ROUND(LocalISC,2).
ASSIGN
    {&Cabecera}.ImpVta = {&Cabecera}.ImpTot - {&Cabecera}.ImpExo - {&Cabecera}.ImpIgv - {&Cabecera}.AcuBon[10].
ASSIGN
    {&Cabecera}.ImpBrt = {&Cabecera}.ImpVta + {&Cabecera}.ImpDto
    {&Cabecera}.SdoAct = {&Cabecera}.ImpTot.
/* RHC 06/05/2014 En caso tenga descuento por Encarte */ 
IF {&Cabecera}.ImpDto2 > 0 THEN DO:
    ASSIGN
        {&Cabecera}.ImpTot = {&Cabecera}.ImpTot - {&Cabecera}.ImpDto2
        {&Cabecera}.ImpIgv = {&Cabecera}.ImpIgv -  ~
              ROUND(LocalDto2xAfectosIgv / ( 1 + {&Cabecera}.PorIgv / 100) * {&Cabecera}.PorIgv / 100, 2)
        {&Cabecera}.ImpExo = {&Cabecera}.ImpExo - LocalDto2xExonerados
        {&Cabecera}.ImpVta = {&Cabecera}.ImpTot - {&Cabecera}.ImpExo - {&Cabecera}.ImpIgv - {&Cabecera}.AcuBon[10].
        {&Cabecera}.ImpBrt = {&Cabecera}.ImpVta + {&Cabecera}.ImpDto.
END.
IF {&Cabecera}.PorIgv = 0.00     /* VENTA INAFECTA */
    THEN ASSIGN
          {&Cabecera}.ImpIgv = 0
          {&Cabecera}.ImpVta = {&Cabecera}.ImpExo
          {&Cabecera}.ImpBrt = {&Cabecera}.ImpExo.

/* ************************************* */
/* RHC 22/07/2016 TRANSFERENCIA GRATUITA */
/* ************************************* */
/* IF {&Cabecera}.FmaPgo = "900" THEN            */
/*     ASSIGN                                    */
/*       {&Cabecera}.ImpBrt = 0                  */
/*       {&Cabecera}.ImpDto = 0                  */
/*       {&Cabecera}.ImpExo = {&Cabecera}.ImpTot */
/*       {&Cabecera}.ImpVta = 0                  */
/*       {&Cabecera}.ImpIgv = 0.                 */
/* ************************************************** */
/* RHC 31/12/2019 Nos aseguramos grabar bien el saldo */
/* ************************************************** */
ASSIGN
    {&Cabecera}.SdoAct = {&Cabecera}.ImpTot.
/* ******************************************** */
/* RHC 15/08/19 IMPUESTO A LA BOLSA DE PLASTICO */
/* ******************************************** */
ASSIGN
    {&Cabecera}.ImpTot = {&Cabecera}.ImpTot /* + {&Cabecera}.AcuBon[10] */
    {&Cabecera}.SdoAct = {&Cabecera}.SdoAct /* + {&Cabecera}.AcuBon[10] */
    .

IF LOOKUP({&Cabecera}.FmaPgo, "899,900") > 0 THEN
    ASSIGN
      {&Cabecera}.SdoAct = 0.

/* IF LOOKUP({&Cabecera}.FmaPgo, "899,900") > 0 THEN                  */
/*     ASSIGN                                                         */
/*       {&Cabecera}.ImpBrt = {&Cabecera}.ImpTot - {&Cabecera}.ImpIgv */
/*       {&Cabecera}.ImpDto = 0                                       */
/*       {&Cabecera}.ImpExo = 0                                       */
/*       {&Cabecera}.ImpVta = 0                                       */
/*       {&Cabecera}.ImpTot = 0                                       */
/*       {&Cabecera}.SdoAct = 0.                                      */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 4.62
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Include 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


