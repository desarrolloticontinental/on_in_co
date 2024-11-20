DEFINE TEMP-TABLE Detalle NO-UNDO LIKE FacTabla.
DEF BUFFER b-Tabla FOR Factabla.
/* POR FAMILIAS */
OUTPUT TO c:\tmp\porfamilias.txt.
FOR EACH FacTabla WHERE FacTabla.CodCia = 1
    AND FacTabla.Tabla = 'CV' NO-LOCK,
    EACH Almtfami WHERE INTEGRAL.Almtfami.CodCia = FacTabla.CodCia
    AND INTEGRAL.Almtfami.codfam = FacTabla.Codigo NO-LOCK:

    RUN Carga-Temporal (FacTabla.Tabla, FacTabla.Codigo).

    FOR EACH Detalle,EACH GN-DIVI WHERE GN-DIVI.CodCia = Detalle.CodCia
        AND GN-DIVI.CodDiv = Detalle.Codigo NO-LOCK:
        DISPLAY
            FacTabla.Codigo COLUMN-LABEL 'Linea'
            Almtfami.desfam
            Detalle.codigo COLUMN-LABEL 'División'
            gn-divi.desdiv
            detalle.valor[1] COLUMN-LABEL '% Com Campaña'
            detalle.valor[2] COLUMN-LABEL '% Com NO Campaña'
            WITH STREAM-IO NO-BOX NO-UNDERLINE WIDTH 320.
        PAUSE 0.
    END.
END.
OUTPUT CLOSE.
/* POR PRODUCTOS */
OUTPUT TO c:\tmp\porproductos.txt.
FOR EACH Almmmatg WHERE Almmmatg.CodCia = 001
    AND Almmmatg.clase <> 'X' AND Almmmatg.libre_f01 >= TODAY:
    DISPLAY
        Almmmatg.codmat 
        Almmmatg.DesMat 
        Almmmatg.DesMar 
        Almmmatg.codfam 
        Almmmatg.subfam
        Almmmatg.clase COLUMN-LABEL 'Com. Dif.'
        Almmmatg.libre_f01 COLUMN-LABEL 'Vencimiento'
        WITH STREAM-IO NO-BOX NO-UNDERLINE WIDTH 320.
END.


OUTPUT close.
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
    FOR EACH B-Tabla NO-LOCK WHERE B-Tabla.codcia = 001
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
