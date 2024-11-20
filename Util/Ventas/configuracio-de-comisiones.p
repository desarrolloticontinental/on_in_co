DEF VAR s-codcia AS INT INIT 001.
DEF VAR s-Tabla AS CHAR INIT 'CV'.
DEFINE BUFFER B-Tabla FOR FacTabla.
DEFINE TEMP-TABLE Detalle NO-UNDO LIKE FacTabla.

OUTPUT TO c:\tmp\porcategoria.txt.
PUT UNFORMATTED 'CATEGORIA|DESCRIPCION|%COM' SKIP.
FOR EACH integral.Porcomi WHERE integral.Porcomi.CodCia = s-codcia NO-LOCK:
    PUT UNFORMATTED
        Catego '|'
        DesCat '|'
        PorCom
        SKIP.
END.
OUTPUT CLOSE.

OUTPUT TO c:\tmp\porfamilias.txt.
PUT UNFORMATTED 'LINEA|DESCRIPCION|DIVISION|DESCRIPCION|%COM CAMPAÑA|%COM NO CAMPAÑA' SKIP.
FOR EACH FacTabla WHERE INTEGRAL.FacTabla.CodCia = s-codcia
    AND INTEGRAL.FacTabla.Tabla = s-Tabla NO-LOCK,
    FIRST Almtfami WHERE INTEGRAL.Almtfami.CodCia = INTEGRAL.FacTabla.CodCia
    AND INTEGRAL.Almtfami.codfam = INTEGRAL.FacTabla.Codigo NO-LOCK:
    RUN Carga-Temporal (FacTabla.Tabla, FacTabla.Codigo).
    FOR EACH Detalle,
        EACH INTEGRAL.GN-DIVI WHERE INTEGRAL.GN-DIVI.CodCia = Detalle.CodCia
        AND INTEGRAL.GN-DIVI.CodDiv = Detalle.Codigo NO-LOCK:
        PUT UNFORMATTED
            factabla.codigo '|'
            almtfami.desfam '|'
            detalle.codigo '|'
            gn-divi.desdiv '|'
            detalle.valor[1] '|'
            detalle.valor[2]
            SKIP.
    END.

END.
OUTPUT CLOSE.

OUTPUT TO c:\tmp\porproducto.txt.
PUT UNFORMATTED 'CODIGO|DESCRIPCION|MARCA|FAMILIA|SUBFAMILIA|COM DIF|VENCIMIENTO' SKIP.
FOR EACH Almmmatg WHERE INTEGRAL.Almmmatg.CodCia = s-codcia NO-LOCK:
    PUT UNFORMATTED
        CodMat '|'
        DesMat '|'
        DesMar '|'
        CodFam '|'
        SubFam '|'
        Clase '|'
        Libre_f01 '|'
        SKIP.
END.
OUTPUT CLOSE.


PROCEDURE Carga-Temporal:

  DEF INPUT PARAMETER pTabla AS CHAR.
  DEF INPUT PARAMETER pCodigo AS CHAR.
  
  DEF VAR s-Tabla LIKE FacTabla.Tabla.
  DEF VAR s-Codigo LIKE FacTabla.Codigo.

  s-Tabla = pTabla.
  s-Codigo= pCodigo.
  FOR EACH Detalle:
    DELETE Detalle.
  END.
  FOR EACH B-Tabla NO-LOCK WHERE B-Tabla.codcia = s-codcia
        AND B-Tabla.Tabla = pTabla
        AND B-Tabla.Codigo BEGINS pCodigo
        AND B-Tabla.Nombre <> '':
    CREATE Detalle.
    ASSIGN
        Detalle.CodCia = B-Tabla.codcia
        Detalle.Codigo = B-Tabla.Nombre
        Detalle.Tabla  = B-Tabla.tabla
        Detalle.Valor[1] = B-Tabla.Valor[1]
        Detalle.Valor[2] = B-Tabla.Valor[2].
  END.

END PROCEDURE.
