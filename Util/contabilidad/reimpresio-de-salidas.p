/* REIMPRESION MANUAL DE MOVIMIENTOS DE SALIDAS DE ALMACEN */
{src/bin/_prns.i}

DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEFINE STREAM Reporte.
DEFINE VAR S-NOMCIA AS CHAR INIT "CONTINENTAL S.A.C.".
DEFINE VAR S-CODALM AS CHAR.
DEFINE VAR S-DESALM AS CHAR.
DEFINE VAR S-USER-ID AS CHAR.

DEF VAR x-Rowid AS ROWID NO-UNDO.

DEFINE VAR S-Procedencia AS CHAR FORMAT "X(55)" INIT "".
DEFINE VAR S-Moneda      AS CHAR FORMAT "X(32)"  INIT "".
DEFINE VAR S-Mon         AS CHAR FORMAT "X(4)"  INIT "".
DEFINE VAR S-Referencia1 AS CHAR FORMAT "X(16)" INIT "".
DEFINE VAR S-Referencia2 AS CHAR FORMAT "X(16)" INIT "".
DEFINE VAR S-Referencia3 AS CHAR FORMAT "X(16)" INIT "".
DEFINE VAR S-Encargado   AS CHAR FORMAT "X(32)" INIT "".
DEFINE VAR S-RUC         AS CHAR FORMAT "X(32)" INIT "".
DEFINE VAR S-Movimiento  AS CHAR FORMAT "X(40)" INIT "".
DEFINE VAR S-TOTAL       AS DECIMAL INIT 0.
DEFINE VAR S-SUBTO       AS DECIMAL INIT 0.
DEFINE VAR S-Item        AS INTEGER INIT 0.
DEFINE VAR W-VTA0        AS DECIMAL INIT 0.
DEFINE VAR W-VTA1        AS DECIMAL INIT 0.
DEFINE VAR W-VTA2        AS DECIMAL INIT 0.

DEF VAR x-linea AS CHAR NO-UNDO.

DEFINE VARIABLE answer AS LOGICAL NO-UNDO.
SYSTEM-DIALOG PRINTER-SETUP UPDATE answer.
IF answer = NO THEN RUN RETURN.

INPUT FROM c:\tmp\salida05.prn.
REPEAT:
    IMPORT UNFORMATTED x-linea.
    IF x-linea = '' THEN LEAVE.
    FIND almcmov WHERE almcmov.codcia = s-codcia
        AND almcmov.tipmov = 'S'
        AND almcmov.codmov = 05
        AND almcmov.codalm = SUBSTRING(x-linea,1,5)
        AND almcmov.nroser = 0
        AND almcmov.nrodoc = INTEGER(SUBSTRING(x-linea,6,10))
        NO-LOCK NO-ERROR.
    IF AVAILABLE almcmov THEN RUN R-IMPFMT.
END.
INPUT CLOSE.


PROCEDURE R-IMPFMT:
/* *************** */

    s-CodAlm = Almcmov.codalm.
    s-Item = 0.

    FIND Almacen WHERE Almacen.CodCia = S-CODCIA AND
         Almacen.CodAlm = S-CODALM NO-LOCK NO-ERROR.
    s-DesAlm = Almacen.Descripcion.
    S-Encargado = Almacen.EncAlm.

    IF almcmov.Codmon = 1 THEN DO:
       S-Moneda = "Compra en       :   SOLES".
       S-Mon    = "S/.".
    END.
    ELSE DO:
       S-Moneda = "Compra en       :   DOLARES".
       S-Mon    = "US$.".
    END.

    FIND FIRST Almtmovm WHERE  Almtmovm.CodCia = Almcmov.CodCia AND
               Almtmovm.Tipmov = Almcmov.TipMov AND 
               Almtmovm.Codmov = Almcmov.CodMov NO-LOCK NO-ERROR.
    IF AVAILABLE Almtmovm THEN DO:
       S-Movimiento = Almcmov.TipMov + STRING(Almcmov.CodMov,"99") + "-" + CAPS(Almtmovm.Desmov).
       IF Almtmovm.PidCli THEN DO:
          FIND gn-clie WHERE gn-clie.CodCia = 0 AND 
               gn-clie.CodCli = Almcmov.CodCli NO-LOCK NO-ERROR.
          IF NOT AVAILABLE gn-clie THEN 
             FIND gn-clie WHERE gn-clie.CodCia = Almcmov.CodCia AND 
                  gn-clie.CodCli = Almcmov.CodCli NO-LOCK NO-ERROR.
          IF AVAILABLE gn-clie THEN DO: 
              S-Procedencia = "Cliente          : " + gn-clie.NomCli.
              S-RUC = "R.U.C.          :   " + gn-clie.Ruc.
          END.
          IF S-RUC = "" THEN S-RUC = "R.U.C.          :   " + Almcmov.CodCli.

       END.
       IF Almtmovm.PidPro THEN DO:
          FIND gn-prov WHERE gn-prov.CodCia = 0 AND 
               gn-prov.CodPro = Almcmov.CodPro NO-LOCK NO-ERROR.
          IF NOT AVAILABLE gn-prov THEN 
            FIND gn-prov WHERE gn-prov.CodCia = Almcmov.CodCia AND 
                 gn-prov.CodPro = Almcmov.CodPro NO-LOCK NO-ERROR.
          IF AVAILABLE gn-prov THEN DO: 
              S-Procedencia = "Proveedor        : " + gn-prov.NomPro.
              S-RUC = "R.U.C.          :   " + gn-prov.Ruc.
          END.
          IF S-RUC = "" THEN S-RUC = "R.U.C.          :   " + Almcmov.CodPro.

       END.
       IF Almtmovm.Movtrf THEN DO:
          S-Moneda = "".
          FIND Almacen WHERE Almacen.CodCia = S-CODCIA AND
               Almacen.CodAlm = Almcmov.AlmDes NO-LOCK NO-ERROR.
          IF AVAILABLE Almacen THEN DO:
             IF Almcmov.TipMov = "I" THEN
                S-Procedencia = "Procedencia      : " + Almcmov.AlmDes + " - " + Almacen.Descripcion.
             ELSE 
                S-Procedencia = "Destino          : " + Almcmov.AlmDes + " - " + Almacen.Descripcion.   
             END.            
       END.   
       IF Almtmovm.PidRef1 THEN ASSIGN S-Referencia1 = Almtmovm.GloRf1.
       IF Almtmovm.PidRef2 THEN ASSIGN S-Referencia2 = Almtmovm.GloRf2.
       IF Almtmovm.PidRef3 THEN ASSIGN S-Referencia3 = Almtmovm.GloRf3.
    END.

    RUN Formato-4.

END PROCEDURE.


PROCEDURE Formato-4:
/* ***************** */

    DEFINE FRAME F-FMT
           S-Item             AT  1   FORMAT "ZZ9"
           Almdmov.CodMat     AT  6   FORMAT "X(6)"
           Almmmatg.DesMat    AT  14  FORMAT "X(50)"
           Almmmatg.Desmar    AT  65  FORMAT "X(15)"
           Almdmov.CodUnd     AT  82  FORMAT "X(4)"          
           Almmmatg.CanEmp    AT  88  FORMAT ">>,>>9.99"
           Almdmov.CanDes     AT  99  FORMAT ">>>,>>9.99" 
           "__________"       AT  111  
           Almmmate.CodUbi    AT  125 FORMAT "X(6)"
           HEADER
           S-NOMCIA FORMAT "X(45)" SKIP
           "( " + S-DESALM + " )" FORMAT "X(50)" 
           S-Movimiento  AT 52  /*"Pag. " AT 108 PAGE-NUMBER(REPORTE) FORMAT ">>9"*/ SKIP
           "( PRE - DESPACHO )" AT 58 SKIP
           "Nro Documento : " AT 105 Almcmov.Nroser AT 121 "-" AT 124 Almcmov.NroDoc FORMAT "9999999" AT 125 SKIP         
           S-PROCEDENCIA AT 1 FORMAT "X(70)" 
           "Fecha : " AT 113 Almcmov.FchDoc AT 121 SKIP
           S-Referencia1 AT 1 ": " Almcmov.NroRf1  AT 20 
           "Almacen Salida  : " AT 79 S-CODALM AT 99       
           "Hora  : " AT 113 Almcmov.HorRcp AT 121 SKIP 
           "Observaciones    : " AT 1 Almcmov.Observ  AT 20 "Responsable     : " AT 79 S-Encargado AT 99 SKIP
           "-----------------------------------------------------------------------------------------------------------------------------------" SKIP
           "ITEM CODIGO                DESCRIPCION                             M A R C A     UND.    EMPAQUE    CANTIDAD  DESPACHADO  UBICACION" SKIP
           "-----------------------------------------------------------------------------------------------------------------------------------" SKIP
  /*           99  999999 12345678901234567890123456789012345678901234567890 123456789012345  81                                       
                 6      13                                                  65 */
           WITH NO-LABEL NO-UNDERLINE NO-BOX WIDTH 145 STREAM-IO DOWN.         

    OUTPUT STREAM Reporte TO PRINTER PAGED PAGE-SIZE 30.
    FIND FIRST Almdmov WHERE Almdmov.CodCia = Almcmov.CodCia AND
               Almdmov.CodAlm = Almcmov.CodAlm AND
               Almdmov.TipMov = Almcmov.TipMov AND
               Almdmov.CodMov = Almcmov.CodMov AND
               Almdmov.NroDoc = Almcmov.NroDoc USE-INDEX Almd01 NO-LOCK NO-ERROR.
    IF AVAILABLE Almdmov THEN DO:
       PUT STREAM Reporte CONTROL {&PRN0} + {&PRN5A} + CHR(33) + {&PRN3}.     
       REPEAT WHILE  AVAILABLE  AlmDMov      AND Almdmov.CodAlm = Almcmov.CodAlm AND
             Almdmov.TipMov = Almcmov.TipMov AND Almdmov.CodMov = Almcmov.CodMov AND
             Almdmov.NroDoc = Almcmov.NroDoc:
             FIND Almmmatg WHERE Almmmatg.CodCia = Almdmov.CodCia AND
                  Almmmatg.CodMat = Almdmov.CodMat NO-LOCK NO-ERROR.
             FIND Almmmate WHERE Almmmate.Codcia = Almdmov.CodCia AND
                  Almmmate.CodAlm = S-CODALM AND
                  Almmmate.CodMat = Almdmov.CodMat NO-LOCK NO-ERROR.
             S-Item = S-Item + 1.     
             DISPLAY STREAM Reporte 
                     S-Item 
                     Almdmov.CodMat 
                     Almdmov.CanDes 
                     Almdmov.CodUnd 
                     Almmmatg.ArtPro 
                     Almmmatg.DesMat 
                     Almmmatg.CanEmp
                     Almmmate.CodUbi
                     Almmmatg.Desmar WITH FRAME F-FMT.
             DOWN STREAM Reporte WITH FRAME F-FMT.
             FIND NEXT Almdmov USE-INDEX Almd01.
       END.
    END.
    DO WHILE LINE-COUNTER(Reporte) < PAGE-SIZE(Reporte) - 4 :
       PUT STREAM Reporte "" skip.
    END.
    PUT STREAM Reporte "                                 -----------------             -----------------          -----------------"  AT 10 SKIP.
    PUT STREAM Reporte "                                    Despachador                     Vo. Bo.                    RECIBE      "  AT 10 SKIP.
    PUT STREAM Reporte  /*Almcmov.usuario AT 18*/ "JEFE ALMACEN         "  AT 75 SKIP.
    PUT STREAM Reporte " ** ALMACEN ** " AT 121 SKIP.
    OUTPUT STREAM Reporte CLOSE.

END PROCEDURE.

