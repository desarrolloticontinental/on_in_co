	��V��hf�>  ? �              +                                �k 3ED000F2utf-8 MAIN D:\newsie\on_in_co\aplic\ccb\dt-nota-cr-db-detail.w,, PROCEDURE Imp-Total,,OUTPUT F-ImpTot DECIMAL PROCEDURE Export-Temp-Table,,INPUT ttCcbDDocu TABLE,OUTPUT pTotal DECIMAL PROCEDURE disable_UI,, PROCEDURE serverCommit,,INPUT-OUTPUT RowObjUpd TABLE,OUTPUT pcMessages CHARACTER,OUTPUT pcUndoIds CHARACTER PROCEDURE remoteCommit,,INPUT-OUTPUT pcContext CHARACTER,INPUT-OUTPUT RowObjUpd TABLE,OUTPUT pcMessages CHARACTER,OUTPUT pcUndoIds CHARACTER PROCEDURE pushTableAndValidate,,INPUT pcValType CHARACTER,INPUT-OUTPUT RowObjUpd TABLE PROCEDURE pushRowObjUpdTable,,INPUT RowObjUpd TABLE PROCEDURE initProps,, PROCEDURE start-super-proc,,INPUT pcProcName CHARACTER PROCEDURE adm-clone-props,, PROCEDURE viewObject,, PROCEDURE toggleData,,INPUT plEnabled LOGICAL PROCEDURE showMessageProcedure,,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL PROCEDURE returnFocus,,INPUT hTarget HANDLE PROCEDURE repositionObject,,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL PROCEDURE removeLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE removeAllLinks,, PROCEDURE modifyUserLinks,,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE PROCEDURE modifyListProperty,,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER PROCEDURE hideObject,, PROCEDURE exitObject,, PROCEDURE editInstanceProperties,, PROCEDURE displayLinks,, PROCEDURE createControls,, PROCEDURE changeCursor,,INPUT pcCursor CHARACTER PROCEDURE applyEntry,,INPUT pcField CHARACTER PROCEDURE adjustTabOrder,,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER PROCEDURE addMessage,,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER PROCEDURE addLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE unbindServer,,INPUT pcMode CHARACTER PROCEDURE runServerObject,,INPUT phAppService HANDLE PROCEDURE disconnectObject,, PROCEDURE destroyObject,, PROCEDURE bindServer,, PROCEDURE transferDBRow,,INPUT pcRowIdent CHARACTER,INPUT piRowNum INTEGER PROCEDURE startFilter,, PROCEDURE releaseDBRow,, PROCEDURE refetchDBRow,,INPUT phRowObjUpd HANDLE PROCEDURE filterContainerHandler,,INPUT phFilterContainer HANDLE PROCEDURE fetchDBRowForUpdate,, PROCEDURE confirmContinue,,INPUT-OUTPUT pioCancel LOGICAL PROCEDURE compareDBRow,, PROCEDURE bufferCopyDBToRO,,INPUT phRowObj HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER PROCEDURE assignDBRow,,INPUT phRowObjUpd HANDLE PROCEDURE updateState,,INPUT pcState CHARACTER PROCEDURE updateQueryPosition,, PROCEDURE updateAddQueryWhere,,INPUT pcWhere CHARACTER,INPUT pcField CHARACTER PROCEDURE undoTransaction,, PROCEDURE transferToExcel,,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT plUseExisting LOGICAL,INPUT piMaxRecords INTEGER PROCEDURE synchronizeProperties,,INPUT pcPropertiesForServer CHARACTER,OUTPUT pcPropertiesForClient CHARACTER PROCEDURE submitValidation,,INPUT pcValueList CHARACTER,INPUT pcUpdColumns CHARACTER PROCEDURE submitForeignKey,,INPUT pcRowIdent CHARACTER,INPUT-OUTPUT pcValueList CHARACTER,INPUT-OUTPUT pcUpdColumns CHARACTER PROCEDURE submitCommit,,INPUT pcRowIdent CHARACTER,INPUT plReopen LOGICAL PROCEDURE startServerObject,, PROCEDURE setPropertyList,,INPUT pcProperties CHARACTER PROCEDURE serverFetchRowObjUpdTable,,OUTPUT phRowObjUpd TABLE-HANDLE PROCEDURE serverSendRows,,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER,OUTPUT phRowObject TABLE-HANDLE PROCEDURE saveContextAndDestroy,,OUTPUT pcContext CHARACTER PROCEDURE rowObjectState,,INPUT pcState CHARACTER PROCEDURE retrieveFilter,, PROCEDURE restartServerObject,, PROCEDURE remoteSendRows,,INPUT-OUTPUT piocContext CHARACTER,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT pioRowsReturned INTEGER,OUTPUT phRowObject TABLE-HANDLE,OUTPUT pocMessages CHARACTER PROCEDURE refreshRow,, PROCEDURE printToCrystal,,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT piMaxRecords INTEGER PROCEDURE isUpdatePending,,INPUT-OUTPUT plUpdate LOGICAL PROCEDURE initializeServerObject,, PROCEDURE initializeObject,, PROCEDURE home,, PROCEDURE genContextList,,OUTPUT pcContext CHARACTER PROCEDURE fetchPrev,, PROCEDURE fetchNext,, PROCEDURE fetchLast,, PROCEDURE fetchFirst,, PROCEDURE fetchBatch,,INPUT plForwards LOGICAL PROCEDURE endClientDataRequest,, PROCEDURE destroyServerObject,, PROCEDURE describeSchema,,INPUT pcSdoName CHARACTER,OUTPUT hTtSchema TABLE-HANDLE PROCEDURE dataAvailable,,INPUT pcRelative CHARACTER PROCEDURE copyColumns,,INPUT pcViewColList CHARACTER,INPUT phDataQuery HANDLE PROCEDURE commitTransaction,, PROCEDURE clientSendRows,,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER PROCEDURE batchServices,,INPUT pcServices CHARACTER,OUTPUT pcValues CHARACTER FUNCTION deleteRecordStatic,logical,INPUT piTableIndex INTEGER FUNCTION getRowObjUpdStatic,widget-handle, FUNCTION Signature,CHARACTER,INPUT pcName CHARACTER FUNCTION showmessage,LOGICAL,INPUT pcMessage CHARACTER FUNCTION setUserProperty,LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION setUIBMode,LOGICAL,INPUT pcMode CHARACTER FUNCTION setTranslatableProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setSupportedLinks,LOGICAL,INPUT pcLinkList CHARACTER FUNCTION setRunAttribute,LOGICAL,INPUT cRunAttribute CHARACTER FUNCTION setPhysicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setPhysicalObjectName,LOGICAL,INPUT cTemp CHARACTER FUNCTION setPassThroughLinks,LOGICAL,INPUT pcLinks CHARACTER FUNCTION setParentDataKey,LOGICAL,INPUT cParentDataKey CHARACTER FUNCTION setObjectVersion,LOGICAL,INPUT cObjectVersion CHARACTER FUNCTION setObjectParent,LOGICAL,INPUT phParent HANDLE FUNCTION setObjectName,LOGICAL,INPUT pcName CHARACTER FUNCTION setLogicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setLogicalObjectName,LOGICAL,INPUT c CHARACTER FUNCTION setInstanceProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setDynamicObject,LOGICAL,INPUT lTemp LOGICAL FUNCTION setDesignDataObject,LOGICAL,INPUT pcDataObject CHARACTER FUNCTION setDBAware,LOGICAL,INPUT lAware LOGICAL FUNCTION setDataTargetEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setDataTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setDataSourceNames,LOGICAL,INPUT pcSourceNames CHARACTER FUNCTION setDataSourceEvents,LOGICAL,INPUT pcEventsList CHARACTER FUNCTION setDataSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDataLinksEnabled,LOGICAL,INPUT lDataLinksEnabled LOGICAL FUNCTION setContainerSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setContainerSource,LOGICAL,INPUT phObject HANDLE FUNCTION setContainerHidden,LOGICAL,INPUT plHidden LOGICAL FUNCTION setChildDataKey,LOGICAL,INPUT cChildDataKey CHARACTER FUNCTION reviewMessages,CHARACTER, FUNCTION propertyType,CHARACTER,INPUT pcPropName CHARACTER FUNCTION messageNumber,CHARACTER,INPUT piMessage INTEGER FUNCTION mappedEntry,CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER FUNCTION linkProperty,CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER FUNCTION linkHandles,CHARACTER,INPUT pcLink CHARACTER FUNCTION instancePropertyList,CHARACTER,INPUT pcPropList CHARACTER FUNCTION getUserProperty,CHARACTER,INPUT pcPropName CHARACTER FUNCTION getUIBMode,CHARACTER, FUNCTION getTranslatableProperties,CHARACTER, FUNCTION getSupportedLinks,CHARACTER, FUNCTION getRunAttribute,CHARACTER, FUNCTION getQueryObject,LOGICAL, FUNCTION getPropertyDialog,CHARACTER, FUNCTION getPhysicalVersion,CHARACTER, FUNCTION getPhysicalObjectName,CHARACTER, FUNCTION getPassThroughLinks,CHARACTER, FUNCTION getParentDataKey,CHARACTER, FUNCTION getObjectVersionNumber,CHARACTER, FUNCTION getObjectVersion,CHARACTER, FUNCTION getObjectParent,HANDLE, FUNCTION getObjectPage,INTEGER, FUNCTION getObjectName,CHARACTER, FUNCTION getObjectInitialized,LOGICAL, FUNCTION getObjectHidden,LOGICAL, FUNCTION getLogicalVersion,CHARACTER, FUNCTION getLogicalObjectName,CHARACTER, FUNCTION getInstanceProperties,CHARACTER, FUNCTION getDynamicObject,LOGICAL, FUNCTION getDesignDataObject,CHARACTER, FUNCTION getDBAware,LOGICAL, FUNCTION getDataTargetEvents,CHARACTER, FUNCTION getDataTarget,CHARACTER, FUNCTION getDataSourceNames,CHARACTER, FUNCTION getDataSourceEvents,CHARACTER, FUNCTION getDataSource,HANDLE, FUNCTION getDataLinksEnabled,LOGICAL, FUNCTION getContainerType,CHARACTER, FUNCTION getContainerSourceEvents,CHARACTER, FUNCTION getContainerSource,HANDLE, FUNCTION getContainerHidden,LOGICAL, FUNCTION getContainerHandle,HANDLE, FUNCTION getChildDataKey,CHARACTER, FUNCTION fetchMessages,CHARACTER, FUNCTION assignLinkProperty,LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION anyMessage,LOGICAL, FUNCTION setServerOperatingMode,LOGICAL,INPUT pcServerOperatingMode CHARACTER FUNCTION setServerFileName,LOGICAL,INPUT pcFileName CHARACTER FUNCTION setASUsePrompt,LOGICAL,INPUT plFlag LOGICAL FUNCTION setASInitializeOnRun,LOGICAL,INPUT plInitialize LOGICAL FUNCTION setASInfo,LOGICAL,INPUT pcInfo CHARACTER FUNCTION setASHandle,LOGICAL,INPUT phASHandle HANDLE FUNCTION setASDivision,LOGICAL,INPUT pcDivision CHARACTER FUNCTION setAppService,LOGICAL,INPUT pcAppService CHARACTER FUNCTION runServerProcedure,HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE FUNCTION getServerOperatingMode,CHARACTER, FUNCTION getServerFileName,CHARACTER, FUNCTION getASUsePrompt,LOGICAL, FUNCTION getASInitializeOnRun,LOGICAL, FUNCTION getASInfo,CHARACTER, FUNCTION getASHasStarted,LOGICAL, FUNCTION getASHandle,HANDLE, FUNCTION getAsDivision,CHARACTER, FUNCTION getASBound,LOGICAL, FUNCTION getAppService,CHARACTER, FUNCTION whereClauseBuffer,CHARACTER,INPUT pcWhere CHARACTER FUNCTION setQueryWhere,LOGICAL,INPUT pcWhere CHARACTER FUNCTION setQueryString,LOGICAL,INPUT pcQueryString CHARACTER FUNCTION setQuerySort,LOGICAL,INPUT pcSort CHARACTER FUNCTION setQueryPosition,LOGICAL,INPUT pcPosition CHARACTER FUNCTION rowidWhereCols,CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER FUNCTION rowidWhere,CHARACTER,INPUT pcWhere CHARACTER FUNCTION resolveBuffer,CHARACTER,INPUT pcBuffer CHARACTER FUNCTION removeQuerySelection,LOGICAL,INPUT pcColumns CHARACTER,INPUT pcOperators CHARACTER FUNCTION removeForeignKey,LOGICAL, FUNCTION refreshRowident,CHARACTER,INPUT pcRowident CHARACTER FUNCTION newWhereClause,CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcExpression CHARACTER,INPUT pcWhere CHARACTER,INPUT pcAndOr CHARACTER FUNCTION newQueryWhere,CHARACTER,INPUT pcWhere CHARACTER FUNCTION newQueryValidate,CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcExpression CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER FUNCTION newQueryString,CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcAndOr CHARACTER FUNCTION insertExpression,CHARACTER,INPUT pcWhere CHARACTER,INPUT pcExpression CHARACTER,INPUT pcAndOr CHARACTER FUNCTION indexInformation,CHARACTER,INPUT pcQuery CHARACTER,INPUT plUseTableSep LOGICAL,INPUT pcIndexInfo CHARACTER FUNCTION getTargetProcedure,HANDLE, FUNCTION getQueryWhere,CHARACTER, FUNCTION getQueryString,CHARACTER, FUNCTION getQuerySort,CHARACTER, FUNCTION getQueryPosition,CHARACTER, FUNCTION getForeignValues,CHARACTER, FUNCTION getDataColumns,CHARACTER, FUNCTION excludeColumns,CHARACTER,INPUT iTable INTEGER FUNCTION dbColumnHandle,HANDLE,INPUT pcColumn CHARACTER FUNCTION dbColumnDataName,CHARACTER,INPUT pcDbColumn CHARACTER FUNCTION columnValMsg,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnTable,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnQuerySelection,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnDbColumn,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnDataType,CHARACTER,INPUT pcColumn CHARACTER FUNCTION bufferWhereClause,CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcWhere CHARACTER FUNCTION bufferCompareDBToRO,LOGICAL,INPUT phRowObjUpd HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER FUNCTION assignQuerySelection,LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER FUNCTION addQueryWhere,LOGICAL,INPUT pcWhere CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER FUNCTION getObjectType,character, FUNCTION updateRow,LOGICAL,INPUT pcKeyValues CHARACTER,INPUT pcValueList CHARACTER FUNCTION submitRow,LOGICAL,INPUT pcRowIdent CHARACTER,INPUT pcValueList CHARACTER FUNCTION rowValues,CHARACTER,INPUT pcColumns CHARACTER,INPUT pcFormat CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION rowAvailable,LOGICAL,INPUT pcDirection CHARACTER FUNCTION prepareQuery,LOGICAL,INPUT pcQuery CHARACTER FUNCTION openQuery,LOGICAL, FUNCTION openDataQuery,LOGICAL,INPUT pcPosition CHARACTER FUNCTION hasForeignKeyChanged,LOGICAL, FUNCTION getLastCommitErrorType,CHARACTER, FUNCTION firstRowIds,CHARACTER,INPUT pcQueryString CHARACTER FUNCTION findRowWhere,LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER FUNCTION findRow,LOGICAL,INPUT pcKeyValues CHARACTER FUNCTION fetchRowIdent,CHARACTER,INPUT pcRowIdent CHARACTER,INPUT pcViewColList CHARACTER FUNCTION fetchRow,CHARACTER,INPUT piRow INTEGER,INPUT pcViewColList CHARACTER FUNCTION deleteRow,LOGICAL,INPUT pcRowIdent CHARACTER FUNCTION createRow,LOGICAL,INPUT pcValueList CHARACTER FUNCTION copyRow,CHARACTER,INPUT pcViewColList CHARACTER FUNCTION colValues,CHARACTER,INPUT pcViewColList CHARACTER FUNCTION columnProps,CHARACTER,INPUT pcColList CHARACTER,INPUT pcPropList CHARACTER FUNCTION closeQuery,LOGICAL, FUNCTION canNavigate,LOGICAL, FUNCTION cancelRow,CHARACTER, FUNCTION addRow,CHARACTER,INPUT pcViewColList CHARACTER TEMP-TABLE ttCcbDDocu 1,CodCia,CodDiv,CodDoc,NroDoc,codmat:llave04 0 NO,CodCia integer 0 0,CodDoc character 1 0,NroDoc character 2 0,NroItm integer 3 0,UndVta character 4 0,codmat character 5 0,PreUni decimal 6 0,PorDto decimal 7 0,ImpDto decimal 9 0,ImpLin decimal 10 0,CanDes decimal 11 0,AftIgv logical 13 0,AftIsc logical 14 0,PreBas decimal 15 0,PreVta decimal[3] 16 0,ImpIgv decimal 17 0,ImpIsc decimal 18 0,Factor decimal 19 0,CanDev decimal 20 0,PorDto2 decimal 8 0,Pesmat decimal 12 0,CodCli character 21 0,AlmDes character 22 0,Por_Dsctos decimal[3] 23 0,Flg_Factor character 24 0,FchDoc date 26 0,CodDiv character 25 0,ImpCto decimal 27 0,puntos decimal 28 0,mrguti decimal 29 0,ImpPro decimal 30 0,ImpDto2 decimal 31 0,PorDcto_Adelanto decimal[5] 32 0,ImpDcto_Adelanto decimal[5] 33 0,Dcto_Otros_Mot character 34 0,Dcto_Otros_Factor decimal 35 0,Dcto_Otros_VV decimal 36 0,Dcto_Otros_PV decimal 37 0,cTipoAfectacion character 38 0,cPreUniSinImpuesto decimal 39 0,FactorDescuento decimal 40 0,TasaIGV decimal 41 0,ImporteUnitarioSinImpuesto decimal 42 0,ImporteReferencial decimal 43 0,ImporteBaseDescuento decimal 44 0,ImporteDescuento decimal 45 0,ImporteTotalSinImpuesto decimal 46 0,MontoBaseIGV decimal 47 0,ImporteIGV decimal 48 0,ImporteTotalImpuestos decimal 49 0,ImporteUnitarioConImpuesto decimal 50 0,cImporteVentaExonerado decimal 51 0,cImporteVentaGratuito decimal 52 0,cSumaImpteTotalSinImpuesto decimal 53 0,cMontoBaseIGV decimal 54 0,cSumaIGV decimal 55 0,cOtrosTributosOpGratuito decimal 56 0,ImpuestoBolsaPlastico decimal 57 0,MontoTributoBolsaPlastico decimal 58 0,CantidadBolsaPlastico integer 59 0,MontoUnitarioBolsaPlastico decimal 60 0,cImporteTotalConImpuesto decimal 61 0,ImporteBaseDescuentoNoAfecto decimal 62 0,FactorDescuentoNoAfecto decimal 63 0,ImporteDescuentoNoAfecto decimal 64 0 TEMP-TABLE RowObjUpd 0,RowNum:RowNum 0 NO,NroItm integer 0 0,codmat character 1 0,DesMat character 2 0,DesMar character 3 0,UndVta character 4 0,CanDes decimal 5 0,ImpLin decimal 6 0,RowNum integer 7 0,RowIdent character 8 0,RowMod character 9 0,RowIdentIdx character 10 0,RowUserProp character 11 0,ChangedFields character 12 0      \L               :             f$ \L  ��              4�              @G  
   +   T� �  W   �� `  X   T� �  Y   �   [   $�   \   <� <  ]   x�    ^   �� 0  `   �� l  a   4� x  b   ? �  �#  iSO8859-1                                                                           �K   # �                                      �                  T�   
             �K  x"    �"   ��   T�  L          l�  �   $L      0L                                                       PROGRESS                         �           
    
                    �              �                                                                                                     
  T         �          X  �B     tC     -  �.�d,D  A                 �    �$          �)      �                INTEGRAL                         PROGRESS                         �     �        �                         _�xc            �  �                              �  �                      4  �  P�     CODMATDESMATCODMARUNDSTKUNDCMPFACEQUCODCTACODNEWMONVTAPREVTAPREBASAFTIGVVINMN1CODCIAVINMN2CODFAMVCTMN1FCHACTCODPR1CODPR2VCTMN2ARTPROFCHUSALFCHUCMPPMAXMN1PMAXMN2PULTMN1PULTMN2USUARIOFCHINGFCHCESFCHALZCLFMATUNDBASSUBFAMCODBRRCODANTTIPARTFCHPRMDFCHPRMHFCHREAPESMATDETALLECANEMPALMACENESDESMARAFTISCPORISCPORVTATPOMRGCTOLISCTOPRMMRGUTIPORMAXFCHMPREUNDANTPREANTPREACTDSCTOSTPOPROPORIGVCTOTOTTPOSUMCTOUNDORDENORDLISORDTMPTPOARTTPOCMBPPCHR__01CHR__02CHR__03DEC__01DEC__02DEC__03DATE__01DATE__02DATE__03MRGUTI-AMRGUTI-BMRGUTI-CPREOFIUNDAUNDBUNDCFLGINTFLGPRECLASEFCHPROMCATCONTATIPROTDSCTOPROMINFORFLGINFORPROMDIVIPROMFCHDPROMFCHHPROMDTODTOVOLRDTOVOLDUNDALTDSCALTMRGALTPREALTLICENCIAPROMMINDIVIPROMMINFCHDPROMMINFCHHPROMMINDTOCODDIGESAVTODIGESALIBRE_C01LIBRE_C02LIBRE_C03LIBRE_C04LIBRE_C05LIBRE_D01LIBRE_D02LIBRE_F01LIBRE_F02STKMINSTKMAXSTKREPDESCRIPCION-LARGADESCRIPCION-TECNICASW-WEBWEB-SUBCATEGORIALIBRE_D03LIBRE_D04LIBRE_D05PESOBRUTOPAQUETELARGOALTOANCHOCTOLISMARCOCTOTOTMARCOCODSSFAMCLFESPECIALFLGCOMERCIALLIBRE_C06LIBRE_C07LIBRE_C08LIBRE_C09LIBRE_C10CODIGOPADREFACTORPADREREQUIERESERIALNRREQUIEREDUEDATEDTOVOLPTASAIMPUESTOIMPORTEUNITARIOSINIMPUESTODTOVOLPSINIMPUESTOIMPORTEUNITARIOSINIMPUESTO_AIMPORTEUNITARIOSINIMPUESTO_BIMPORTEUNITARIOSINIMPUESTO_CDTOVOLPIMPUESTOIMPORTEUNITARIOIMPUESTOIMPORTEUNITARIOIMPUESTO_AIMPORTEUNITARIOIMPUESTO_BIMPORTEUNITARIOIMPUESTO_C                                                                      	          
                                                                                                                                                                                                                                      !          "          #          $          %          &          '          (          )          *          +          ,          -          .          /          0          1          2         3          4          5         6          7          8         9          :          ;          <         =          >          ?          @          A          B          C          D          E          F          G          H          I          J          K          L          M          N          O          P          Q          R          S          T          U          V          W          X          Y          Z          [         \         ]         ^         _         `         a 
        b 
        c 
        d 
        e 
        f 
        g         h         i         j         k         l 
        m 
        n 
        o 
        p          q          r          s          t          u          v          w          x          y          z          {          |          }          ~                    �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          � 
        �          �          � 
        �          �          �          � 
        �          �          �          �          x  �      �  
    
                  �  �             d                                                                                          �          
  $  �      �  
    
                  �  T                                                                                                       �          
  �  �      L  
    
                  8                �                                                                                          �          
  |  �      �  
    
                  �  �             h                                                                                          �          
  (  �      �  
    
                  �  X  	                                                                                                     �          
  �  �      P  
    
                  <    
           �                                                                                          �          
  �  		      �  
    
                  �  �             l                                                                                          		          
  ,  	      �  
    
                  �  \                                                                                                       	          
  �  -	      T                         @               �                                                                                          -	            �  :	                               �  �             p                                                                                          :	            0  H	      �  
    
                  �  `                                                                                                       H	          
  �  V	      X  
    
                  D               �                                                                                          V	          
  �  d	        
    
                  �  �             t                                                                                          d	          
  4  r	      �                        �  d                                                                                                        r	            �  �	      \                        H               �                                                                                          �	            �  �	                              �  �             x                                                                                          �	                �	      �                        �  8             $                                                                                          �	            �         �       /  H  �!     �!  /  �      H"         /             �          �      �            �       K  X  �J      K  K  ��      \K         K         �    0E          HF      �       "       �       y#  B  �!     �!  /  �      H"         y#  �  b      �          �      �                                                                                                                                                             	                  
                                                                                       �  �  �  �  �                         �  �  �     �                                  4   (           8              T   \   d   t   l                          x   �   �   �   �           �              �   �   �   �   �                          �   �    !   !  !                         $!  ,!  8!  @!                             D!  P!  X!  d!                              h!  p!  x!  �!                             �!  �!  �!  �!                             �!  �!  �!  �!                                                                          NroItm  >>9 No  No  0   codmat  X(8)    Articulo    Articulo        DesMat  X(100)  Descripci�n Descripci�n     Descripci�n del material    DesMar  X(30)   Marca   Marca       UndVta  x(8)    Unidad  Unidad      Unidad de movimiento    CanDes  >,>>>,>>9.9999  Cantidad    Cantidad    0   ImpLin  ->>,>>>,>>9.99  Importe!con IGV Importe!con IGV 0   RowNum  ->,>>>,>>9  RowNum  0   RowIdent    x(8)    RowIdent        RowMod  x(8)    RowMod      RowIdentIdx x(8)    RowIdentIdx     RowUserProp x(8)    RowUserProp     �  ���������                   �!        �!        �!                �     i     i     i    	 	 	 	    0   >   �!  �!  7   i   b   �!  �!  �!  �!  "                ��                                               ��           $  L$  L l�"                �         
             
             
                                         
                                                                                                               
             
                                          L   \   l   |   �   �   �   �   �   �   �   �       ,  <  L  \      L   \   l   |   �   �   �   �   �   �   �   �      ,  <  L  \                                                                                                                                     	                                    
                                                                                                                                                                                                                                                                                                                                                                                                           !                 "                 #                  $                  %                  &                  '                  (                  )                  *                  +                  ,                  -                  .                  /                  0                  1                  2                  3                  4                  5                  6                  7                  8                  9                  :                  ;                  <                  =                  >                  ?                  @                  A                  B                                 5   5  $5  ,5  (5                         05  85  @5  P5  H5                         T5  \5  d5  t5  l5                         x5  �5  �5  �5  �5                         �5  �5  �5  �5  �5          �5              �5  �5  �5   6  �5                         6  6   6  @6  06                         D6  L6  T6  l6  `6                         p6  x6  �6  �6  �6                         �6  �6  �6  �6  �6                         �6  �6  �6  7   7                         7  7  $7  <7  07                         @7  H7  X7  h7  `7                         l7  t7  |7  �7  �7                          �7  �7  �7  �7  �7                          �7  �7  �7  �7  �7                         �7  �7   8   8  8                         $8  ,8  <8  T8  H8                         X8  `8  p8  �8  |8                         �8  �8  �8  �8  �8          �8             �8  �8  �8  �8  �8                         �8  �8   9  9  9                         9  9  $9  L9  89          P9              d9  p9  �9  �9  �9                         �9  �9  �9  �9                              �9  �9  �9  �9  �9                         �9  �9  �9  :  �9                         :  :  $:  4:  ,:                         8:  @:  L:  T:                             X:  `:  l:  t:                             x:  �:  �:  �:                             �:  �:  �:  �:                             �:  �:  �:  �:                             �:  ;  ;  0;                             4;  D;  L;  \;                              `;  t;  �;  �;                             �;  �;  �;  �;                             �;  �;  �;  <                             <  <  $<  4<                              8<  L<  d<  x<                       
      |<  �<  �<  �<                             �<  �<  �<  �<                             �<  �<   =  =                       
       =  4=  L=  `=                       
      d=  |=  �=  �=                             �=  �=  �=  �=                             �=  �=  >  $>                             (>  8>  H>  X>                             \>  h>  x>  �>                             �>  �>  �>  �>                             �>  �>  ?   ?                       
      $?  <?  P?  h?                             l?  �?  �?  �?                             �?  �?  �?  �?                              @  @   @  0@                             4@  @@  P@  \@                             `@  |@  �@  �@                             �@  �@  �@  �@                             �@  A  A  8A                             <A  TA  `A  xA                              |A  �A  �A  �A                             �A  �A  �A  B                             B  8B  HB  hB                             lB  �B  �B  �B                             �B  �B  �B  �B                                                                         CodCia  999 Cia Cia 0   CodDoc  x(3)    Codigo  Codigo      NroDoc  X(12)   Numero  Numero      NroItm  >>9 No.Item No.Item 0   UndVta  x(8)    Unidad  Und     Unidad de movimiento    codmat  X(6)    Codigo Articulo Codigo Articulo     PreUni  >,>>>,>>9.999999    Precio Unitario Precio Unitario 0   PorDto  >>9.99  % Dscto.    % Dscto.    0   PorDto2 >>9.99  % Dscto.    % Dscto.    0   ImpDto  >,>>>,>>9.9999  Importe Descuento   Importe!Descuento   0   ImpLin  ->>,>>>,>>9.99  Importe Importe 0   CanDes  >,>>>,>>9.9999  Cantidad    Cantidad    0   Pesmat  ->>,>>9.9999    Peso    Peso    0   AftIgv  Si/No   I.G.V.  I.G.V.  Si  AftIsc  Si/No   I.S.C.  I.S.C.  No  PreBas  >,>>>,>>9.9999  Precio Base Precio Base 0   PreVta  >>,>>>,>>9.99   Precio Venta    Precio Venta    0   ImpIgv  >,>>>,>>9.9999  Importe Igv Importe Igv 0   ImpIsc  >,>>>,>>9.9999  Importe Isc Importe Isc 0   Factor  >>,>>9.9999 Factor  Factor  0   Factor  CanDev  >,>>>,>>9.9999  Cantidad    Cantidad    0   CodCli  x(11)   Cliente Cliente     AlmDes  x(3)    Almac�n Despacho    Almac�n!Despacho        Almac�n despacho    Por_Dsctos  ->,>>9.999999   % Dscto % Dscto 0   Flg_Factor  X(1)    Flg_Factor      CodDiv  XX-XXX  C.Div   C.Div   00000   FchDoc  99/99/9999  Fecha   Fecha   TODAY   ImpCto  ->>,>>>,>>9.99  ImpCto  ImpCto  0   puntos  ->>,>>9.99  puntos  0   mrguti  ->>,>>9.99  mrguti  0   ImpPro  ->>>,>>>,>>9.99 ImpPro  0   ImpDto2 ->>>,>>>,>>9.99 ImpDto2 0   PorDcto_Adelanto    ->>,>>9.99  PorDcto_Adelanto    0   ImpDcto_Adelanto    >>>,>>>,>>9.99  ImpDcto_Adelanto    0   Dcto_Otros_Mot  x(20)   Dcto_Otros_Mot      Dcto_Otros_Factor   ->>,>>9.999999  Dcto_Otros_Factor   0   Dcto_Otros_VV   ->>>,>>>,>>9.999999 Dcto_Otros_VV   0   Dcto_Otros_PV   ->>>,>>>,>>9.999999 Dcto_Otros_PV   0   cTipoAfectacion x(25)   cTipoAfectacion     cPreUniSinImpuesto  >>>>>>>>>>>9.9999999999 cPreUniSinImpuesto  0   FactorDescuento >>9.99999   FactorDescuento 0   TasaIGV >>9.99999   TasaIGV 0   ImporteUnitarioSinImpuesto  >>>>>>>>>>>9.9999999999 ImporteUnitarioSinImpuesto  0   ImporteReferencial  >>>>>>>>>>>9.9999999999 ImporteReferencial  0   ImporteBaseDescuento    >>>>>>>>>>>9.99 ImporteBaseDescuento    0   ImporteDescuento    >>>>>>>>>>>9.99 ImporteDescuento    0   ImporteTotalSinImpuesto >>>>>>>>>>>9.99 ImporteTotalSinImpuesto 0   MontoBaseIGV    >>>>>>>>>>>9.99 MontoBaseIGV    0   ImporteIGV  >>>>>>>>>>>9.99 ImporteIGV  0   ImporteTotalImpuestos   >>>>>>>>>>>9.99 ImporteTotalImpuestos   0   ImporteUnitarioConImpuesto  >>>>>>>>>>>9.99999999999    ImporteUnitarioConImpuesto  0   cImporteVentaExonerado  >>>,>>>,>>9.9999    cImporteVentaExonerado  0   cImporteVentaGratuito   >>>,>>>,>>9.9999    cImporteVentaGratuito   0   cSumaImpteTotalSinImpuesto  >>>,>>>,>>9.99  cSumaImpteTotalSinImpuesto  0   cMontoBaseIGV   >>>,>>>,>>9.99  cMontoBaseIGV   0   cSumaIGV    >>>,>>>,>>9.99  cSumaIGV    0   cOtrosTributosOpGratuito    >>>,>>>,>>9.99  cOtrosTributosOpGratuito    0   ImpuestoBolsaPlastico   >>>,>>>,>>9.99  ImpuestoBolsaPlastico   0   MontoTributoBolsaPlastico   >>>,>>>,>>9.99  MontoTributoBolsaPlastico   0   CantidadBolsaPlastico   ->,>>>,>>9  CantidadBolsaPlastico   0   MontoUnitarioBolsaPlastico  >>>,>>>,>>9.9999    MontoUnitarioBolsaPlastico  0   cImporteTotalConImpuesto    >>>,>>>,>>9.99  cImporteTotalConImpuesto    0   ImporteBaseDescuentoNoAfecto    >>>>>>>>>>>9.99 ImporteBaseDescuentoNoAfecto    0   FactorDescuentoNoAfecto >>9.99  FactorDescuentoNoAfecto 0   ImporteDescuentoNoAfecto    >>>>>>>>>>>9.99 ImporteDescuentoNoAfecto    0   �    A a q�  ���B������             �    �        �    � �00000     �      ��      �                                   �#        �#        �#        �#                �     i  i  i  i      i  i  i      i  i  i  i  i      i  i  i  i  i     	 	 	 	 	 	 	 	       "   )   0   7   >   E   L   [   b   i   w   ~   �   �   �   �   �   �   S   p   �   �   �   �   �   �   �   �   �   �   �       '  6  H  V  d  t  �  �  �  �  �  �  �      #  9  T  k  �  �  �  �  �  �  �    -  F  c  {                                                                                                                                     	                  
                                                                                                         �H  �H  �H  �H  �H                         �H  �H  �H  �H  �H                         �H  �H  I  I  I           I             <I  DI  LI  \I  TI                         `I  hI  pI  �I  xI          �I             �I  �I  �I  �I  �I                         �I  �I  �I  J  �I                         J  J   J  (J                             ,J  8J  @J  LJ                              PJ  XJ  `J  hJ                             lJ  xJ  �J  �J                             �J  �J  �J  �J                              �J  �J  �J  �J                                                                          NroItm  >>9 No  No  0   codmat  X(8)    Articulo    Articulo        DesMat  X(100)  Descripci�n Descripci�n     Descripci�n del material    DesMar  X(30)   Marca   Marca       UndVta  x(8)    Unidad  Unidad      Unidad de movimiento    CanDes  >,>>>,>>9.9999  Cantidad    Cantidad    0   ImpLin  ->>,>>>,>>9.99  Importe!con IGV Importe!con IGV 0   RowNum  ->,>>>,>>9  RowNum  0   RowIdent    x(8)    RowIdent        RowMod  x(8)    RowMod      RowIdentIdx x(8)    RowIdentIdx     RowUserProp x(8)    RowUserProp     ChangedFields   x(8)    ChangedFields       �  ���������                   �!        �!        �!                �     i     i     i    	 	 	 	    0   >   �!  �!  7   i   b   �!  �!  �!  �!  "  "    ��                            ����                            ]!    ��              ��    ��    �#   ��                    �    �!  "       undefined                                                               �       ��  �   l   ��  ��                    �����               �b�                    O   ����    e�          O   ����    R�          O   ����    ��      t       �   �              4   ����     /                                    3   ����       $     H  ���                       8      
                       � ߱        �  �      D       �     @          ��    �   �  4      d       4   ����d                 D                      ��                  �   �                   ���                       �   �  �  	  �   x                                        3   ����|       O   �   ��  ��  �   batchServices                               4        ��                  ]  `  L              $��                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             d               ��                  �           ��                            ����                            clientSendRows                              �  p      ��                  b  h  �              �s�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             �               ��                �               ��   <                            ��   d             0               ��                  X           ��                            ����                            commitTransaction                               X  @      ��                  j  k  p              �E�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            copyColumns                             X  @      ��                  m  p  p              �F�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             �               �� 
                 �  
         ��                            ����                            dataAvailable                               �  �      ��                  r  t  �              L��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            describeSchema                              �	  �	      ��                  v  y  �	              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��   <
             
               �� 
          �       0
  
         ��                            ����                            destroyServerObject                             0        ��                  {  |  H              ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            endClientDataRequest                                <  $      ��                  ~    T              ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchBatch                              <  $      ��                  �  �  T              `��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  l           ��                            ����                            fetchFirst                              d  L      ��                  �  �  |               �                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchLast                               d  L      ��                  �  �  |              x�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchNext                               d  L      ��                  �  �  |              �                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchPrev                               d  L      ��                  �  �  |              (�-                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            genContextList                              h  P      ��                  �  �  �              ��-                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            home                                �  t      ��                  �  �  �              @�-                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �  |      ��                  �  �  �              @�-                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              �  �      ��                  �  �  �              ��-                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            isUpdatePending                             �  �      ��                  �  �  �              ��-                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            printToCrystal                              �  �      ��                  �  �  �              ��-                    O   ����    e�          O   ����    R�          O   ����    ��            ��   4                             ��   \             (               ��                  P           ��                            ����                            refreshRow                              H  0      ��                  �  �  `              ��-                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            remoteSendRows                              L  4      ��                  �  �  d              `�-                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             |               ��   �             �               ��                 �               ��   (             �               ��   P                            ��   x             D               �� 
  �      �       l  
             ��                  �           ��                            ����                            restartServerObject                             �  |      ��                  �  �  �              �.                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            retrieveFilter                              �  �      ��                  �  �  �              ��-                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            rowObjectState                              �  �      ��                  �  �  �               �-                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            saveContextAndDestroy                               �  �      ��                  �  �  �              �.                    O   ����    e�          O   ����    R�          O   ����    ��            ��                               ��                            ����                            serverSendRows                              �   �       ��                  �  �  !              ��-                    O   ����    e�          O   ����    R�          O   ����    ��            ��   `!             ,!               ��   �!             T!               ��   �!             |!               ��   �!             �!               ��    "             �!               �� 
          �       �!  
         ��                            ����                            serverFetchRowObjUpdTable                               �"  �"      ��                  �  �  #              ��-                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
          �       ,#  
         ��                            ����                            setPropertyList                             ($  $      ��                  �  �  @$              @�-                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  X$           ��                            ����                            serverSendRows                              T%  <%      ��                  �  �  l%              .                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �%             �%               ��   �%             �%               ��   &             �%               ��   0&             �%               ��   X&             $&               �� 
          �       L&  
         ��                            ����                            startServerObject                               L'  4'      ��                  �  �  d'               �.                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            submitCommit                                P(  8(      ��                  �  �  h(              Є.                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �(             �(               ��                  �(           ��                            ����                            submitForeignKey                                �)  �)      ��                  �  �  �)              ȍ.                    O   ����    e�          O   ����    R�          O   ����    ��            ��   *             �)               ��   4*              *               ��                  (*           ��                            ����                            submitValidation                                (+  +      ��                  �     @+               0.                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �+             X+               ��                  �+           ��                            ����                            synchronizeProperties                               �,  l,      ��                      �,              �*.                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �,             �,               ��                  �,           ��                            ����                            transferToExcel                             �-  �-      ��                      �-              X�.                    O   ����    e�          O   ����    R�          O   ����    ��            ��   <.             .               ��   d.             0.               ��   �.             X.               ��                  �.           ��                            ����                            undoTransaction                             |/  d/      ��                      �/              DB.                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            updateAddQueryWhere                             �0  l0      ��                      �0              �D.                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �0             �0               ��                  �0           ��                            ����                            updateQueryPosition                             �1  �1      ��                      �1              ��.                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            updateState                             �2  �2      ��                  !  #  �2              �.                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  3           ��                            ����                            addRow          t3      �3           CHARACTER,INPUT pcViewColList CHARACTER cancelRow   |3      �3      �3   	       CHARACTER,  canNavigate �3      �3      (4          LOGICAL,    closeQuery  4      44      `4   
 $      LOGICAL,    columnProps @4      l4      �4    /      CHARACTER,INPUT pcColList CHARACTER,INPUT pcPropList CHARACTER  colValues   x4      �4      5   	 ;      CHARACTER,INPUT pcViewColList CHARACTER copyRow �4      ,5      T5    E      CHARACTER,INPUT pcViewColList CHARACTER createRow   45      |5      �5   	 M      LOGICAL,INPUT pcValueList CHARACTER deleteRow   �5      �5      �5   	 W      LOGICAL,INPUT pcRowIdent CHARACTER  fetchRow    �5      6      H6  	  a      CHARACTER,INPUT piRow INTEGER,INPUT pcViewColList CHARACTER fetchRowIdent   (6      �6      �6  
  j      CHARACTER,INPUT pcRowIdent CHARACTER,INPUT pcViewColList CHARACTER  findRow �6      �6       7    x      LOGICAL,INPUT pcKeyValues CHARACTER findRowWhere     7      D7      t7    �      LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER  firstRowIds T7      �7      �7    �      CHARACTER,INPUT pcQueryString CHARACTER getLastCommitErrorType  �7       8      X8    �      CHARACTER,  hasForeignKeyChanged    88      d8      �8    �      LOGICAL,    openDataQuery   |8      �8      �8    �      LOGICAL,INPUT pcPosition CHARACTER  openQuery   �8      �8      (9   	 �      LOGICAL,    prepareQuery    9      49      d9    �      LOGICAL,INPUT pcQuery CHARACTER rowAvailable    D9      �9      �9    �      LOGICAL,INPUT pcDirection CHARACTER rowValues   �9      �9      :   	 �      CHARACTER,INPUT pcColumns CHARACTER,INPUT pcFormat CHARACTER,INPUT pcDelimiter CHARACTER    submitRow   �9      `:      �:   	       LOGICAL,INPUT pcRowIdent CHARACTER,INPUT pcValueList CHARACTER  updateRow   l:      �:      �:   	       LOGICAL,INPUT pcKeyValues CHARACTER,INPUT pcValueList CHARACTER getObjectType   �:      8;      h;          CHARACTER,  assignDBRow                             <  �;      ��                  	    <              D�.                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 4<  
         ��                            ����                            bufferCopyDBToRO                                4=  =      ��                      L=              H�.                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �=             d=  
             �� 
  �=             �=  
             ��   �=             �=               ��                  �=           ��                            ����                            compareDBRow                                �>  �>      ��                      �>              �/                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            confirmContinue                             �?  �?      ��                      �?              x/                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  @           ��                            ����                            dataAvailable                               A  �@      ��                       A              \�.                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  8A           ��                            ����                            fetchDBRowForUpdate                             8B   B      ��                       PB              $/                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchFirst                              8C   C      ��                  "  #  PC              �/                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchLast                               8D   D      ��                  %  &  PD              D�.                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchNext                               8E   E      ��                  (  )  PE              ��.                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchPrev                               8F   F      ��                  +  ,  PF              X�.                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            filterContainerHandler                              DG  ,G      ��                  .  0  \G               �.                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 tG  
         ��                            ����                            initializeObject                                tH  \H      ��                  2  3  �H              ��.                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            refetchDBRow                                xI  `I      ��                  5  7  �I              H/                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �I  
         ��                            ����                            releaseDBRow                                �J  �J      ��                  9  :  �J              8�.                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            startFilter                             �K  �K      ��                  <  =  �K              ��.                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            transferDBRow                               �L  �L      ��                  ?  B  �L              ��.                    O   ����    e�          O   ����    R�          O   ����    ��            ��   M             �L               ��                   M           ��                            ����                            addQueryWhere   H;      hM      �M    6      LOGICAL,INPUT pcWhere CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER    assignQuerySelection    xM      �M      $N    D      LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER  bufferCompareDBToRO N      |N      �N    Y      LOGICAL,INPUT phRowObjUpd HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER bufferWhereClause   �N      O      PO    m      CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcWhere CHARACTER  columnDataType  0O      �O      �O          CHARACTER,INPUT pcColumn CHARACTER  columnDbColumn  �O      �O      P    �      CHARACTER,INPUT pcColumn CHARACTER  columnQuerySelection    �O      4P      lP    �      CHARACTER,INPUT pcColumn CHARACTER  columnTable LP      �P      �P    �      CHARACTER,INPUT pcColumn CHARACTER  columnValMsg    �P      �P      Q     �      CHARACTER,INPUT pcColumn CHARACTER  dbColumnDataName    �P      4Q      hQ  !  �      CHARACTER,INPUT pcDbColumn CHARACTER    dbColumnHandle  HQ      �Q      �Q  "  �      HANDLE,INPUT pcColumn CHARACTER excludeColumns  �Q      �Q      R  #  �      CHARACTER,INPUT iTable INTEGER  getDataColumns  �Q      0R      `R  $  �      CHARACTER,  getForeignValues    @R      lR      �R  %  	      CHARACTER,  getQueryPosition    �R      �R      �R  &        CHARACTER,  getQuerySort    �R      �R      S  '  +      CHARACTER,  getQueryString  �R      (S      XS  (  8      CHARACTER,  getQueryWhere   8S      dS      �S  )  G      CHARACTER,  getTargetProcedure  tS      �S      �S  *  U      HANDLE, indexInformation    �S      �S      T  +  h      CHARACTER,INPUT pcQuery CHARACTER,INPUT plUseTableSep LOGICAL,INPUT pcIndexInfo CHARACTER   insertExpression    �S      lT      �T  ,  y      CHARACTER,INPUT pcWhere CHARACTER,INPUT pcExpression CHARACTER,INPUT pcAndOr CHARACTER  newQueryString  �T      �T      (U  -  �      CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcAndOr CHARACTER  newQueryValidate    U      �U      �U  .  �      CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcExpression CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER   newQueryWhere   �U      dV      �V  /  �      CHARACTER,INPUT pcWhere CHARACTER   newWhereClause  tV      �V      �V  0  �      CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcExpression CHARACTER,INPUT pcWhere CHARACTER,INPUT pcAndOr CHARACTER refreshRowident �V      XW      �W  1  �      CHARACTER,INPUT pcRowident CHARACTER    removeForeignKey    hW      �W      �W  2  �      LOGICAL,    removeQuerySelection    �W      �W      (X  3  �      LOGICAL,INPUT pcColumns CHARACTER,INPUT pcOperators CHARACTER   resolveBuffer   X      hX      �X  4  �      CHARACTER,INPUT pcBuffer CHARACTER  rowidWhere  xX      �X      �X  5 
       CHARACTER,INPUT pcWhere CHARACTER   rowidWhereCols  �X      Y      <Y  6        CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER    setQueryPosition    Y      �Y      �Y  7  %      LOGICAL,INPUT pcPosition CHARACTER  setQuerySort    �Y      �Y       Z  8  6      LOGICAL,INPUT pcSort CHARACTER  setQueryString   Z      @Z      pZ  9  C      LOGICAL,INPUT pcQueryString CHARACTER   setQueryWhere   PZ      �Z      �Z  :  R      LOGICAL,INPUT pcWhere CHARACTER whereClauseBuffer   �Z      �Z      [  ;  `      CHARACTER,INPUT pcWhere CHARACTER   bindServer                              �[  �[      ��                  �  �  �[              x�/                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �\  �\      ��                  �  �  �\              �/                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyServerObject                             �]  �]      ��                  �  �  �]              h�/                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disconnectObject                                �^  �^      ��                  �  �  �^              0�/                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              �_  �_      ��                  �  �  `              (r/                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            restartServerObject                             �`  �`      ��                  �  �  a              �r/                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            runServerObject                             �a  �a      ��                  �  �  b              �!/                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 ,b  
         ��                            ����                            startServerObject                               ,c  c      ��                  �  �  Dc              dN/                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            unbindServer                                0d  d      ��                  �  �  Hd              �Q/                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  `d           ��                            ����                            getAppService   �Z      �d      �d  <  r      CHARACTER,  getASBound  �d      e      0e  = 
 �      LOGICAL,    getAsDivision   e      <e      le  >  �      CHARACTER,  getASHandle Le      xe      �e  ?  �      HANDLE, getASHasStarted �e      �e      �e  @  �      LOGICAL,    getASInfo   �e      �e      f  A 	 �      CHARACTER,  getASInitializeOnRun    �e       f      Xf  B  �      LOGICAL,    getASUsePrompt  8f      df      �f  C  �      LOGICAL,    getServerFileName   tf      �f      �f  D  �      CHARACTER,  getServerOperatingMode  �f      �f      g  E  �      CHARACTER,  runServerProcedure  �f      $g      Xg  F        HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE   setAppService   8g      �g      �g  G        LOGICAL,INPUT pcAppService CHARACTER    setASDivision   �g      �g      $h  H  -      LOGICAL,INPUT pcDivision CHARACTER  setASHandle h      Hh      th  I  ;      LOGICAL,INPUT phASHandle HANDLE setASInfo   Th      �h      �h  J 	 G      LOGICAL,INPUT pcInfo CHARACTER  setASInitializeOnRun    �h      �h      i  K  Q      LOGICAL,INPUT plInitialize LOGICAL  setASUsePrompt  �h      <i      li  L  f      LOGICAL,INPUT plFlag LOGICAL    setServerFileName   Li      �i      �i  M  u      LOGICAL,INPUT pcFileName CHARACTER  setServerOperatingMode  �i      �i      j  N  �      LOGICAL,INPUT pcServerOperatingMode CHARACTER   addLink                             �j  �j      ��                  �  �  �j              �z/                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  <k             k  
             ��   dk             0k               �� 
                 Xk  
         ��                            ����                            addMessage                              Pl  8l      ��                  �  �  hl              l�/                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �l             �l               ��   �l             �l               ��                  �l           ��                            ����                            adjustTabOrder                              �m  �m      ��                  �  �  �m              ��/                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  0n             �m  
             �� 
  Xn             $n  
             ��                  Ln           ��                            ����                            applyEntry                              Do  ,o      ��                  �  �  \o              �/                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  to           ��                            ����                            changeCursor                                pp  Xp      ��                  �  �  �p              d
0                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �p           ��                            ����                            createControls                              �q  �q      ��                  �  �  �q              ��/                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �r  �r      ��                  �  �  �r              �/                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            displayLinks                                �s  �s      ��                  �  �  �s              ,�/                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            editInstanceProperties                              �t  �t      ��                  �  �  �t              0�/                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            exitObject                              �u  �u      ��                  �  �  �u              ��/                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hideObject                              �v  �v      ��                  �  �  �v              |�/                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �w  �w      ��                  �  �  �w              ��/                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            modifyListProperty                              �x  �x      ��                  �  �  �x              ̣/                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  $y             �x  
             ��   Ly             y               ��   ty             @y               ��                  hy           ��                            ����                            modifyUserLinks                             dz  Lz      ��                  �  �  |z              X0                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �z             �z               ��   �z             �z               �� 
                 �z  
         ��                            ����                            removeAllLinks                              �{  �{      ��                  �  �  �{              �0                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            removeLink                              �|  �|      ��                  �  �  �|              T0                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  D}             }  
             ��   l}             8}               �� 
                 `}  
         ��                            ����                            repositionObject                                `~  H~      ��                  �    x~              ��/                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �~             �~               ��                  �~           ��                            ����                            returnFocus                             �  �      ��                      �              ��/                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �  
         ��                            ����                            showMessageProcedure                                �  ̀      ��                      ��              �/                    O   ����    e�          O   ����    R�          O   ����    ��            ��   H�             �               ��                  <�           ��                            ����                            toggleData                              4�  �      ��                      L�              ��/                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  d�           ��                            ����                            viewObject                              \�  D�      ��                      t�              �/                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            anyMessage  �i      ̃      ��  O 
 �	      LOGICAL,    assignLinkProperty  ؃      �      8�  P  �	      LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER   fetchMessages   �      ��      ��  Q  

      CHARACTER,  getChildDataKey ��      ̄      ��  R  
      CHARACTER,  getContainerHandle  ܄      �      <�  S  (
      HANDLE, getContainerHidden  �      D�      x�  T  ;
      LOGICAL,    getContainerSource  X�      ��      ��  U  N
      HANDLE, getContainerSourceEvents    ��      ��      ��  V  a
      CHARACTER,  getContainerType    ܅      �      <�  W  z
      CHARACTER,  getDataLinksEnabled �      H�      |�  X  �
      LOGICAL,    getDataSource   \�      ��      ��  Y  �
      HANDLE, getDataSourceEvents ��      ��      �  Z  �
      CHARACTER,  getDataSourceNames  Ԇ       �      4�  [  �
      CHARACTER,  getDataTarget   �      @�      p�  \  �
      CHARACTER,  getDataTargetEvents P�      |�      ��  ]  �
      CHARACTER,  getDBAware  ��      ��      �  ^ 
 �
      LOGICAL,    getDesignDataObject ȇ      �      (�  _        CHARACTER,  getDynamicObject    �      4�      h�  `        LOGICAL,    getInstanceProperties   H�      t�      ��  a  &      CHARACTER,  getLogicalObjectName    ��      ��      ��  b  <      CHARACTER,  getLogicalVersion   Ј      ��      0�  c  Q      CHARACTER,  getObjectHidden �      <�      l�  d  c      LOGICAL,    getObjectInitialized    L�      x�      ��  e  s      LOGICAL,    getObjectName   ��      ��      �  f  �      CHARACTER,  getObjectPage   ̉      ��      (�  g  �      INTEGER,    getObjectParent �      4�      d�  h  �      HANDLE, getObjectVersion    D�      l�      ��  i  �      CHARACTER,  getObjectVersionNumber  ��      ��      �  j  �      CHARACTER,  getParentDataKey    Ċ      ��      $�  k  �      CHARACTER,  getPassThroughLinks �      0�      d�  l  �      CHARACTER,  getPhysicalObjectName   D�      p�      ��  m        CHARACTER,  getPhysicalVersion  ��      ��      �  n        CHARACTER,  getPropertyDialog   ȋ      �      (�  o  *      CHARACTER,  getQueryObject  �      4�      d�  p  <      LOGICAL,    getRunAttribute D�      p�      ��  q  K      CHARACTER,  getSupportedLinks   ��      ��      ��  r  [      CHARACTER,  getTranslatableProperties   ��      �      (�  s  m      CHARACTER,  getUIBMode  �      4�      `�  t 
 �      CHARACTER,  getUserProperty @�      l�      ��  u  �      CHARACTER,INPUT pcPropName CHARACTER    instancePropertyList    |�      č      ��  v  �      CHARACTER,INPUT pcPropList CHARACTER    linkHandles ܍      $�      P�  w  �      CHARACTER,INPUT pcLink CHARACTER    linkProperty    0�      t�      ��  x  �      CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER mappedEntry ��      ��      �  y  �      CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER  messageNumber   �      x�      ��  z  �      CHARACTER,INPUT piMessage INTEGER   propertyType    ��      ̏      ��  {  �      CHARACTER,INPUT pcPropName CHARACTER    reviewMessages  ܏      $�      T�  |  �      CHARACTER,  setChildDataKey 4�      `�      ��  }        LOGICAL,INPUT cChildDataKey CHARACTER   setContainerHidden  p�      ��      �  ~        LOGICAL,INPUT plHidden LOGICAL  setContainerSource  ̐      �      @�    )      LOGICAL,INPUT phObject HANDLE   setContainerSourceEvents     �      `�      ��  �  <      LOGICAL,INPUT pcEvents CHARACTER    setDataLinksEnabled |�      ��      ��  �  U      LOGICAL,INPUT lDataLinksEnabled LOGICAL setDataSource   ԑ      �      L�  �  i      LOGICAL,INPUT phObject HANDLE   setDataSourceEvents ,�      l�      ��  �  w      LOGICAL,INPUT pcEventsList CHARACTER    setDataSourceNames  ��      Ȓ      ��  �  �      LOGICAL,INPUT pcSourceNames CHARACTER   setDataTarget   ܒ      $�      T�  �  �      LOGICAL,INPUT pcTarget CHARACTER    setDataTargetEvents 4�      x�      ��  �  �      LOGICAL,INPUT pcEvents CHARACTER    setDBAware  ��      Г      ��  � 
 �      LOGICAL,INPUT lAware LOGICAL    setDesignDataObject ܓ      �      P�  �  �      LOGICAL,INPUT pcDataObject CHARACTER    setDynamicObject    0�      x�      ��  �  �      LOGICAL,INPUT lTemp LOGICAL setInstanceProperties   ��      Ȕ       �  �  �      LOGICAL,INPUT pcPropList CHARACTER  setLogicalObjectName    ��      $�      \�  �        LOGICAL,INPUT c CHARACTER   setLogicalVersion   <�      x�      ��  �        LOGICAL,INPUT cVersion CHARACTER    setObjectName   ��      Е       �  �  -      LOGICAL,INPUT pcName CHARACTER  setObjectParent ��       �      P�  �  ;      LOGICAL,INPUT phParent HANDLE   setObjectVersion    0�      p�      ��  �  K      LOGICAL,INPUT cObjectVersion CHARACTER  setParentDataKey    ��      ̖       �  �  \      LOGICAL,INPUT cParentDataKey CHARACTER  setPassThroughLinks ��      (�      \�  �  m      LOGICAL,INPUT pcLinks CHARACTER setPhysicalObjectName   <�      |�      ��  �  �      LOGICAL,INPUT cTemp CHARACTER   setPhysicalVersion  ��      ԗ      �  �  �      LOGICAL,INPUT cVersion CHARACTER    setRunAttribute �      ,�      \�  �  �      LOGICAL,INPUT cRunAttribute CHARACTER   setSupportedLinks   <�      ��      ��  �  �      LOGICAL,INPUT pcLinkList CHARACTER  setTranslatableProperties   ��      ܘ      �  �  �      LOGICAL,INPUT pcPropList CHARACTER  setUIBMode  ��      <�      h�  � 
 �      LOGICAL,INPUT pcMode CHARACTER  setUserProperty H�      ��      ��  �  �      LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER  showmessage ��      ��      $�  �        LOGICAL,INPUT pcMessage CHARACTER   Signature   �      H�      t�  � 	       CHARACTER,INPUT pcName CHARACTER    l�    (  ��  0�      �       4   �����                 @�                      ��                  )  V                  �)0                       )  Ě        *  \�  ؛      �       4   �����                 �                      ��                  +  U                  @*0                       +  l�  �    B  �  ��      �       4   �����                 ��                      ��                  N  P                  �-0                       N  �         O                                  ,     
                    � ߱        �  $  R  ��  ���                           $  T  @�  ���                       x                         � ߱        x�    Z  ��  �      �      4   �����                �                      ��                  [  	                  x.0                       [  ��  H�  o   ^      ,                                 ��  $   _  t�  ���                       �  @         �              � ߱        ��  �   `        Ȟ  �   a  �      ܞ  �   c        �  �   e  x      �  �   g  �      �  �   i  `      ,�  �   j  �      @�  �   k        T�  �   n  �      h�  �   p         |�  �   q  |      ��  �   s  �      ��  �   t  t      ��  �   u  �      ̟  �   v  ,      ��  �   w  �      ��  �   }  �      �  �     P	      �  �   �  �	      0�  �   �   
      D�  �   �  t
      X�  �   �  �
      l�  �   �  l      ��  �   �  �      ��  �   �  \      ��  �   �  �      ��  �   �  D      Р  �   �  �      �  �   �  �      ��  �   �  0      �  �   �  �       �  �   �  �      4�  �   �        H�  �   �  X      \�  �   �  �      p�  �   �        ��  �   �  L      ��  �   �  �      ��  �   �  �      ��  �   �         ԡ  �   �  <      �  �   �  x      ��  �   �  �      �  �   �  �          �   �  ,                      <�          ��  ��      ��                  F	  t	  ��              �,                    O   ����    e�          O   ����    R�          O   ����    ��      �     
                                     (                         � ߱        h�  $ Z	  آ  ���                           O   r	  ��  ��  h               ԣ          ģ  ̣    ��                                             ��                            ����                                8;      $�      ��     V     ܣ                       أ                       8�    �	  ��  �      t      4   ����t                 �                      ��                  �	  
                  ��,                       �	  ��  4�  �   �	  �      H�  �   �	  H      \�  �   �	  �      p�  �   �	  @      ��  �   �	  �      ��  �   �	  8      ��  �   �	  �      ��  �   �	  (      ԥ  �   �	  �      �  �   �	         ��  �   �	  �      �  �   �	        $�  �   �	  �          �   �	        ��    K
  T�  Ц      x      4   ����x                �                      ��                  L
  �
                  ��/                       L
  d�  ��  �   N
  �      �  �   O
  T      �  �   P
  �      0�  �   Q
  D      D�  �   R
  �      X�  �   S
  �      l�  �   U
  p      ��  �   V
  �      ��  �   W
  X      ��  �   X
  �      ��  �   Y
  �      Ч  �   Z
  D       �  �   [
  �       ��  �   \
  �       �  �   ]
  x!       �  �   ^
  �!      4�  �   _
  h"      H�  �   `
  �"      \�  �   a
  `#      p�  �   b
  �#      ��  �   c
  X$      ��  �   d
  �$      ��  �   e
  �$      ��  �   f
  L%      Ԩ  �   g
  �%      �  �   h
  <&      ��  �   i
  �&      �  �   j
  4'      $�  �   k
  �'      8�  �   l
  ,(      L�  �   m
  h(      `�  �   o
  �(      t�  �   p
  X)      ��  �   q
  �)      ��  �   r
  *      ��  �   s
  �*      ĩ  �   t
  �*      ة  �   u
  l+      �  �   v
  �+       �  �   w
  \,      �  �   x
  �,      (�  �   y
  L-      <�  �   z
  �-      P�  �   {
  <.      d�  �   |
  �.      x�  �   }
  4/      ��  �   ~
  �/          �   
  $0      d�    �
  ��  8�      T0      4   ����T0                H�                      ��                  �
  �                  -                       �
  ̪  \�  �   �
  �0      p�  �   �
  (1      ��  �   �
  �1      ��  �      2      ��  �     �2      ��  �     3      ԫ  �     |3      �  �     �3      ��  �     t4      �  �     �4      $�  �     l5      8�  �     �5      L�  �   	  d6      `�  �   
  �6      t�  �     L7      ��  �     �7      ��  �     <8      ��  �     �8      Ĭ  �     ,9      ج  �     �9      �  �     :       �  �     X:      �  �     �:      (�  �     H;      <�  �     �;      P�  �     8<          �     �<      x�    �  ��  ��      =      4   ����=  	              �                      ��             	     �  7                  �/                       �  ��   �  �   �  |=      4�  �   �  �=      H�  �   �  t>      \�  �   �  �>      p�  �   �  l?      ��  �   �  �?      ��  �   �  \@      ��  �   �  �@      ��  �   �  TA      Ԯ  �   �  �A      �  �   �  DB      ��  �   �  �B      �  �   �  <C      $�  �   �  �C      8�  �   �  ,D      L�  �   �  �D      `�  �   �  $E      t�  �   �  �E      ��  �   �  F      ��  �   �  �F      ��  �   �  G      į  �   �  �G      د  �   �  �G      �  �   �  8H       �  �   �  �H      �  �   �  0I      (�  �   �  �I      <�  �   �   J          �   �  �J      getRowObjUpdStatic  deleteRecordStatic  �    �  ��  ��      K      4   ����K      /   �  а     �                          3   ����K             �                      3   ����<K  Զ    �  ,�  ��  �  XK      4   ����XK  
              ��                      ��             
     �  Y                  t-                       �  <�  ̱  �   �  �K      $�  $  �  ��  ���                       �K     
                    � ߱        8�  �   �  L      ��  $   �  d�  ���                       ,L  @         L              � ߱        L�  $    ��  ���                       �L       	       	           � ߱        �M     
                N                     \O  @        
 O              � ߱        ܳ  V     �  ���                        hO       	       	       �O       
       
       �O       	       	           � ߱        l�  $  (  x�  ���                       �P     
                Q                     dR  @        
 $R              � ߱            V   :  �  ���                                      ̵                      ��                  [  �                  �-                       [  ��  pR     
                �R                     <T  @        
 �S          �T  @        
 dT          U  @        
 �T          dU  @        
 $U              � ߱            V   p  �  ���                        adm-clone-props �  ��              �     W     `                          \                       start-super-proc    �  d�  �           �     X                                  :                     l�      �   �      �X      4   �����X      /     ,�     <�                          3   ���� Y            \�                      3   ���� Y  ķ  $  +  ��  ���                       @Y                         � ߱        ��    ;  �  \�  ��  \Y      4   ����\Y                и                      ��                  <  @                  ��,                       <  �  pY                     �Y                     �Y                         � ߱            $  =  l�  ���                             A  �  T�      �Y      4   �����Y  �Y                         � ߱            $  B  (�  ���                       |�    I  ��  ��  �  �Y      4   �����Y      $  J  ع  ���                       Z                         � ߱            �   g  Z      XZ     
                �Z                     $\  @        
 �[              � ߱        ��  V   {  �  ���                        ��  �   �  0\      ��    -  غ  �      p\      4   ����p\      /   .  �     $�                          3   �����\            D�                      3   �����\  �\     
                <]                     �^  @        
 L^              � ߱        H�  V   :  T�  ���                        �^     
                T_                     �`  @        
 d`              � ߱        t�  V   ^  �  ���                        ��    �  ��  �      �`      4   �����`                �                      ��                  �  �                  ��,                       �  ��  ��  /   �  H�     X�                          3   �����`            x�                      3   �����`      /   �  ��     Ľ                          3   ����a            �                      3   ����$a  p�  /  K   �         Xa                      3   ����@a  initProps   x�  0�              �     Y     \             �          X  h!  	                                   t�          �  �      ��                �  �  4�              ��                    O   ����    e�          O   ����    R�          O   ����    ��      r!                      L�          H�  p   �  �}  ��      �  ��  �     �}                �                      ��                  �  �                  ���                       �  ��  4�  :  �                 $  �  `�  ���                       �}                         � ߱        ��  �     �}                �                      ��                  �  �                  l��                       �  ��  0�  :  �                 $  �  \�  ���                       �}                         � ߱        �  �     �}                                        ��                  �                    Ե�                       �  ��  ��  ��     ~                                        ��                    +                  ���                         $�  ,�  �     $~                                        ��                  ,  H                  t��                       ,  ��  ��  ��     8~                                        ��                  I  e                  ���                       I  <�  D�  4�     L~                                        ��                  f  �                  P��                       f  ��  ��  ��     `~                                        ��                  �  �                   ��                       �  T�  \�  L�     t~  	                                      ��             	     �  �                  �                       �  ��  ��  ��     �~  
                                      ��             
     �  �                  Tą                       �  l�  t�  d�     �~                                        ��                  �  �                  �ą                       �  ��   �  ��     �~                                        ��                  �                    �Ņ                       �  ��  ��  |�     �~                                        ��                    0                  xƅ                         �  �  �     �~                                        ��                  1  M                  Hǅ                       1  ��  ��  ��     �~                                        ��                  N  j                  \ȅ                       N  (�  0�   �                                              ��                  k  �                  $Ʌ                       k  ��  ��  ��                                             ��                  �  �                  �Ʌ                       �  @�      8�     (                                        ��                  �  �                  �ʅ                       �  ��      O   �  ��  ��  <               ��          ��  ��   , ��                                                       �     ��                            ����                            <�  d�  X�  ��      `�     Z     ��                      � ��  �!                     ,�    �  ��  �      H      4   ����H                �                      ��                  �  �                  D��                       �  ��  ��  /   �  D�     T�                          3   ����X            t�                      3   ����x  ��  /   �  ��     ��                          3   �����            ��                      3   �����  \�  /   �  �     ,�                          3   �����            L�                      3   �����      /   �  ��     ��                          3   �����            ��                      3   ����,�  L�     
                Ȁ                     �  @        
 ؁              � ߱        X�  V   @  ��  ���                        �  $  Z  ��  ���                       ,�                         � ߱        X�     
                Ԃ                     $�  @        
 �              � ߱        @�  V   d  ��  ���                        ��  $  ~  l�  ���                       0�     
                    � ߱        D�     
                ��                     �  @        
 Ѕ              � ߱        (�  V   �  ��  ���                        ��  $  �  T�  ���                       �     
                    � ߱        0�     
                ��                     ��  @        
 ��              � ߱        �  V   �  ��  ���                        ��  $  �  <�  ���                       �                         � ߱        <�     
                ��                     �  @        
 ȉ              � ߱        ��  V   �  h�  ���                        �  �   �   �      ��  $  �  8�  ���                       @�     
                    � ߱        T�     
                Њ                      �  @        
 ��              � ߱        ��  V   �  d�  ���                        L�  $     �  ���                       ,�     
                    � ߱        `�  �   *  @�      ��  $  4  ��  ���                       ��     
                    � ߱        ��  �   N  ��      $�  $  p  ��  ���                       Ԍ                         � ߱              {  @�  P�      ��      4   ������      /   |  |�     ��                          3   �����  ��     
   ��                      3   ����0�  ��        ��                      3   ����8�  �        �                      3   ����L�            <�                      3   ����h�  pushRowObjUpdTable  �  L�  �                   [      �                               �"                     pushTableAndValidate    `�  ��  �           |     \     �                          �  �"                     remoteCommit    ��  0�  �           p     ]     �                          �  #                     serverCommit    @�  ��  �           l     ^     �                          �  "#                                     ��          ��  t�      ��                  �  �  ��              �R�                    O   ����    e�          O   ����    R�          O   ����    ��          O   �  ��  ��  ��    ��                            ����                            ��  P�      �              _      ��                      
�     /#                     disable_UI  �  p�                      `      �                               B#  
                   Export-Temp-Table   |�  ��  �           �      a     ,                          (  ^#                     Imp-Total   ��  H�  �           �    ! b     �                          �  �#  	                    �  �    ����  �       ��             ��    /   �  8   ����   �  8   ����   (�  8   ����   8�  8   ����   H�  8   ����   X�  8   ����       8   ����       8   ����       x�  ��      viewObject  ,   h�  ��  ��      toggleData  ,INPUT plEnabled LOGICAL    ��  ��  ��      showMessageProcedure    ,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL  ��  ,�  8�      returnFocus ,INPUT hTarget HANDLE   �  `�  t�      repositionObject    ,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL    P�  ��  ��      removeLink  ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE ��  �   �      removeAllLinks  ,    �  4�  D�      modifyUserLinks ,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE $�  ��  ��      modifyListProperty  ,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER    ��  (�  4�      hideObject  ,   �  H�  T�      exitObject  ,   8�  h�  ��      editInstanceProperties  ,   X�  ��  ��      displayLinks    ,   ��  ��  ��      createControls  ,   ��  ��  ��      changeCursor    ,INPUT pcCursor CHARACTER   ��  �  $�      applyEntry  ,INPUT pcField CHARACTER    �  P�  `�      adjustTabOrder  ,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER @�  ��  ��      addMessage  ,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER ��  �  $�      addLink ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE �  x�  ��      unbindServer    ,INPUT pcMode CHARACTER h�  ��  ��      runServerObject ,INPUT phAppService HANDLE  ��  ��   �      disconnectObject    ,   ��  �  $�      destroyObject   ,   �  8�  D�      bindServer  ,   (�  X�  h�      transferDBRow   ,INPUT pcRowIdent CHARACTER,INPUT piRowNum INTEGER  H�  ��  ��      startFilter ,   ��  ��  ��      releaseDBRow    ,   ��  ��   �      refetchDBRow    ,INPUT phRowObjUpd HANDLE   ��  ,�  D�      filterContainerHandler  ,INPUT phFilterContainer HANDLE �  t�  ��      fetchDBRowForUpdate ,   d�  ��  ��      confirmContinue ,INPUT-OUTPUT pioCancel LOGICAL ��  ��  ��      compareDBRow    ,   ��   �  �      bufferCopyDBToRO    ,INPUT phRowObj HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER   ��  ��  ��      assignDBRow ,INPUT phRowObjUpd HANDLE   x�  ��  ��      updateState ,INPUT pcState CHARACTER    ��  ��  �      updateQueryPosition ,   ��   �  4�      updateAddQueryWhere ,INPUT pcWhere CHARACTER,INPUT pcField CHARACTER    �  x�  ��      undoTransaction ,   h�  ��  ��      transferToExcel ,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT plUseExisting LOGICAL,INPUT piMaxRecords INTEGER  ��  ,�  D�      synchronizeProperties   ,INPUT pcPropertiesForServer CHARACTER,OUTPUT pcPropertiesForClient CHARACTER   �  ��  ��      submitValidation    ,INPUT pcValueList CHARACTER,INPUT pcUpdColumns CHARACTER   ��  �  �      submitForeignKey    ,INPUT pcRowIdent CHARACTER,INPUT-OUTPUT pcValueList CHARACTER,INPUT-OUTPUT pcUpdColumns CHARACTER  ��  ��  ��      submitCommit    ,INPUT pcRowIdent CHARACTER,INPUT plReopen LOGICAL  |�  ��  ��      startServerObject   ,   ��  �  �      setPropertyList ,INPUT pcProperties CHARACTER   ��  H�  d�      serverFetchRowObjUpdTable   ,OUTPUT TABLE-HANDLE phRowObjUpd    8�  ��  ��      serverSendRows  ,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER,OUTPUT TABLE-HANDLE phRowObject    ��  `�  x�      saveContextAndDestroy   ,OUTPUT pcContext CHARACTER P�  ��  ��      rowObjectState  ,INPUT pcState CHARACTER    ��  ��  ��      retrieveFilter  ,   ��  �  �      restartServerObject ,   ��  ,�  <�      remoteSendRows  ,INPUT-OUTPUT piocContext CHARACTER,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT pioRowsReturned INTEGER,OUTPUT TABLE-HANDLE phRowObject,OUTPUT pocMessages CHARACTER   �  4�  @�      refreshRow  ,   $�  T�  d�      printToCrystal  ,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT piMaxRecords INTEGER  D�  ��  ��      isUpdatePending ,INPUT-OUTPUT plUpdate LOGICAL  ��  �   �      initializeServerObject  ,   ��  4�  H�      initializeObject    ,   $�  \�  d�      home    ,   L�  x�  ��      genContextList  ,OUTPUT pcContext CHARACTER h�  ��  ��      fetchPrev   ,   ��  ��  ��      fetchNext   ,   ��  ��   �      fetchLast   ,   ��  �   �      fetchFirst  ,   �  4�  @�      fetchBatch  ,INPUT plForwards LOGICAL   $�  l�  ��      endClientDataRequest    ,   \�  ��  ��      destroyServerObject ,   ��  ��  ��      describeSchema  ,INPUT pcSdoName CHARACTER,OUTPUT TABLE-HANDLE hTtSchema    ��  �  ,�      dataAvailable   ,INPUT pcRelative CHARACTER �  X�  d�      copyColumns ,INPUT pcViewColList CHARACTER,INPUT phDataQuery HANDLE H�  ��  ��      commitTransaction   ,   ��  ��  ��      clientSendRows  ,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER    ��  |�  ��      batchServices   ,INPUT pcServices CHARACTER,OUTPUT pcValues CHARACTER        � 
"     
 �%     adecomm/as-utils.w 
"   
   �    }        �
"     
    �     }        �� �  R   %               � 
" 
   
 %              h �P  \         (          
�                          
�            �    �
" 
   
 0
�H T   %              �     }        �GG %              � 
"   
   P �L 
�H T   %              �     }        �GG %              
"   
   �        �    7%               
"   
 7�               1� '  
 7� 2   %               o%   o           � 7    7
"   
 7�           �    1� 8   7� 2   %               o%   o           � F   7
"   
 7�           �    1� M  
 7� 2   %               o%   o           � X   7
"   
 7�           l    1� h   7� 2   %               o%   o           � 7    7
"   
 7�           �    1� v   7� 2   %               o%   o           � �   7
"   
 7�           T    1� �   7� �   %               o%   o           %               
"   
 �          �    1� �   � �     
"   
 7�               1� �   7� 2   %               o%   o           � �  7
"   
 7�           �    1� �   7� 2   %               o%   o           � �  S 7
"   
 7�           �    1� =   7� �   %               o%   o           %               
"   
 7�           p    1� M   7� �   %               o%   o           %               
"   
 7�           �    1� _   7� �   %               o%   o           %              
"   
 �          h    1� l   � �     
"   
 7�           �    1� {  
 7� �   %               o%   o           %               
"   
 7�                1� �   7� 2   %               o%   o           � 7    7
"   
 �          �    1� �   � �     
"   
 7�           �    1� �   7� 2   %               o%   o           � �  t 7
"   
 �          D	    1� )  
 � �     
"   
 7�           �	    1� 4   7� 2   %               o%   o           � E  � 7
"   
 7�           �	    1� �   7� 2   %               o%   o           � 7    7
"   
 7�           h
    1� �  
 7� �   %               o%   o           %               
"   
 ,�           �
    1� �   ,� �   %               o%   o           %              
"   
 0�           `    1�     0� 2   %               o%   o           � 7    ,
"   
 0�           �    1�    0� 2   %               o%   o           o%   o           
"   
 ��           P    1� !  
 �� 2   %               o%   o           � 7    0
"   
 0�           �    1� ,   0� =  	 %               o%   o           � G  / �
"   
 �          8    1� w   � =  	   
"   
 0�           t    1� �   0� =  	 o%   o           o%   o           � 7    0
"   
 �          �    1� �   � =  	   
"   
 0�           $    1� �   0� =  	 o%   o           o%   o           � 7    0
"   
 �          �    1� �   � �     
"   
 �          �    1� �   � =  	   
"   
 �              1� �   � =  	   
"   
 �          L    1� �   � =  	   
"   
 0�           �    1� �   0� �   o%   o           o%   o           %              
"   
 �              1�    � =  	   
"   
 �          @    1�   
 �      
"   
 �          |    1� #   � =  	   
"   
 �          �    1� 2   � =  	   
"   
 �          �    1� E   � =  	   
"   
 �          0    1� Z   � =  	   
"   
 �          l    1� i  	 � =  	   
"   
 �          �    1� s   � =  	   
"   
 �          �    1� �   � =  	   
"   
 0�                1� �   0� 2   %               o%   o           o%   o           
�H T   %              �     }        �GG %              
"   
   
"   
 ,
"   
   
"   
 �(�  L ( l       �        �    �� �   � P   �        �    �@    
� @  , 
�            �� �     p�               �L
�    %              � 8          � $         � �          
�    � �     
"   
 �� @  , 
�           �� M  
 �p�               �L"      P �L 
�H T   %              �     }        �GG %              
"   
 ,�           �    1� �  
 ,� 2   %               o%   o           � 7    ,
"   
 ,�           <    1� �  
 ,� 2   %               o%   o           o%   o           
"   
 ,�           �    1� �   ,� �   %               o%   o           o%   o           
"   
 0�           4    1� �   0� �   %               o%   o           %               
"   
 ,�           �    1�    ,� �   %               o%   o           %               
"   
 ��           ,    1�    �� 2   %               o%   o           � 7    ,
"   
 0�           �    1�    0� �   %               o%   o           %              
"   
 0�               1� *   0� �   %               o%   o           o%   o           
"   
 ��           �    1� 6   �� 2   %               o%   o           o%   o           
"   
 ,�               1� D  	 ,� 2   %               o%   o           � 7    0
"   
 ,�           �    1� N   ,� 2   %               o%   o           o%   o           
"   
 ,�               1� b   ,� 2   %               o%   o           o%   o           
"   
 ,�           �    1� q   ,� �   %               o%   o           %               
"   
 ,�           �    1� �   ,� �   %               o%   o           o%   o           P �L 
�H T   %              �     }        �GG %              
"   
 ,�           �    1� �  
 ,� �   %               o%   o           %              
"   
 ,�           H    1� �   ,� 2   %               o%   o           o%   o           
"   
 ,�           �    1� �   ,� 2   %               o%   o           � 7    �
"   
 ,�           8    1� �   ,� 2   %               o%   o           o%   o           
"   
 �          �    1� �   � �     
"   
 0�           �    1� �   0� 2   %               o%   o           � �  ! ,
"   
 0�           d    1�     0� 2   %               o%   o           � 7    0
"   
 0�           �    1�    0� 2   %               o%   o           �     0
"   
 �          L    1� /   � <     
"   
 �          �    1� B   � �     
"   
 ,�           �    1� V   ,� 2   %               o%   o           � 7    ,
"   
 �          8     1� b  
 � �     
"   
 ��           t     1� m   �� �   %               o%   o           o%   o           
"   
 0�           �     1� {   0� �   %               o%   o           %               
"   
 -�           l!    1� �   -� �   %               o%   o           %               
"   
 ��           �!    1� �   �� 2   %               o%   o           � 7    -
"   
 ��           \"    1� �   �� 2   %               o%   o           o%   o           
"   
 ,�           �"    1� �   ,� �   %               o%   o           %              
"   
 0�           T#    1� �   0� �   %               o%   o           %               
"   
 ��           �#    1� �   �� �   %               o%   o           %               
"   
 �          L$    1� �   � �     
"   
 �          �$    1� �   � 2     
"   
 0�           �$    1� �   0� �   %               o%   o           o%   o           
"   
 ��           @%    1� 	   �� 2   %               o%   o           � 7    0
"   
 ��           �%    1�    �� 2   %               o%   o           o%   o           
"   
 ,�           0&    1� %   ,� �   o%   o           o%   o           o%   o           
"   
 ,�           �&    1� :   ,� =  	 %               o%   o           o%   o           
"   
 ,�           ('    1� K   ,� 2   %               o%   o           o%   o           
"   
 ,�           �'    1� X  
 ,� �   %               o%   o           o%   o           
"   
 �           (    1� c   � 2     
"   
 0�           \(    1� t   0� 2   %               o%   o           � �  4 -
"   
 0�           �(    1� �  
 0� �   %               o%   o           %              
"   
 �          L)    1� �   � �     
"   
 ,�           �)    1� �   ,� 2   %               o%   o           � 7    �
"   
 0�           �)    1� �   0� �   %               o%   o           %              
"   
 ��           x*    1� �   �� 2   %               o%   o           � 7    0
"   
 ,�           �*    1�    ,� 2   %               o%   o           � 7    �
"   
 -�           `+    1�    -� 2   %               o%   o           � 7    ,
"   
 ,�           �+    1�     ,� �   %               o%   o           %               
"   
 ,�           P,    1� /  	 ,� �   %               o%   o           o%   o           
"   
 ��           �,    1� 9   �� 2   %               o%   o           � H  	 0
"   
 ,�           @-    1� R   ,� �   %               o%   o           %       �       
"   
 0�           �-    1� ^   0� 2   %               o%   o           � 7    ,
"   
 0�           0.    1� e   0� �   o%   o           o%   o           %              
"   
 ,�           �.    1� w   ,� �   %               o%   o           %               
"   
 ,�           (/    1� �   ,� 2   %               o%   o           o%   o           
"   
 ,�           �/    1� �   ,� =  	 %               o%   o           � 7    ,
"   
 �          0    1� �   � =  	   P �L 
�H T   %              �     }        �GG %              
"   
 ��           �0    1� �  
 �� 2   %               o%   o           � 7    �
"   
 ,�           1    1� �   ,� �   %               o%   o           %               
"   
 0�           �1    1� �  	 0� 2   %               o%   o           � 7    ,
"   
 0�           2    1� �   0� 2   %               o%   o           � 7    0
"   
 ,�           �2    1� �   ,� �   %               o%   o           %               
"   
 -�           �2    1� �   -� 2   %               o%   o           � 7    ,
"   
 -�           p3    1�    -� 2   %               o%   o           o%   o           
"   
 ,�           �3    1�    ,� 2   %               o%   o           o%   o           
"   
 0�           h4    1� %   0� �   %               o%   o           o%   o           
"   
 ��           �4    1� 3   �� �   %               o%   o           o%   o           
"   
 ,�           `5    1� C   ,� �   %               o%   o           o%   o           
"   
 0�           �5    1� T   0� 2   %               o%   o           o%   o           
"   
 ,�           X6    1� c  	 ,� =  	 %               o%   o           � 7    0
"   
 ��           �6    1� m  
 �� =  	 %               o%   o           � 7    ,
"   
 ,�           @7    1� x   ,� 2   %               o%   o           � 7    �
"   
 ,�           �7    1� �   ,� 2   %               o%   o           o%   o           
"   
 ,�           08    1� �   ,� 2   %               o%   o           o%   o           
"   
 ,�           �8    1� �   ,� 2   %               o%   o           � 7    -
"   
 0�            9    1� �   0� 2   %               o%   o           � 7    ,
"   
 0�           �9    1� �   0� =  	 %               o%   o           o%   o           
"   
 �          :    1� �   � �     
"   
 0�           L:    1� �   0� 2   %               o%   o           � 7    ,
"   
 0�           �:    1� �   0� 2   %               o%   o           o%   o           
"   
 ��           <;    1�    �� �   %               o%   o           o%   o           
"   
 -�           �;    1�   
 -� 2   %               o%   o           � 7    ,
"   
 ,�           ,<    1� "   ,� 2   %               o%   o           � 7    -
"   
 ��           �<    1� :   �� �   %               o%   o           %               P �L 
�H T   %              �     }        �GG %              
"   
 ��           p=    1� K  	 �� �   %               o%   o           o%   o           
"   
 0�           �=    1� U   0� �   %               o%   o           o%   o           
"   
 ,�           h>    1� d   ,� �   %               o%   o           o%   o           
"   
 ��           �>    1� s   �� �   %               o%   o           %              
"   
 ,�           `?    1� �   ,� 2   %               o%   o           � �  M �
"   
 -�           �?    1� �   -� �   %               o%   o           %              
"   
 ,�           P@    1� �   ,� �   %               o%   o           %               
"   
 0�           �@    1�    0� �   %               o%   o           %               
"   
 ,�           HA    1� *   ,� =  	 %               o%   o           � 8   0
"   
 ,�           �A    1� U   ,� �   %               o%   o           %               
"   
 ,�           8B    1� d   ,� =  	 %               o%   o           o%   o           
"   
 ,�           �B    1� q   ,� �   o%   o           o%   o           %              
"   
 ��           0C    1� �   �� =  	 o%   o           o%   o           � 7    �
"   
 ,�           �C    1� �   ,� �   o%   o           o%   o           o%   o           
"   
 ,�            D    1� �   ,� �   o%   o           o%   o           o%   o           
"   
 ,�           �D    1� �   ,� =  	 o%   o           o%   o           o%   o           
"   
 ,�           E    1� �   ,� �   o%   o           o%   o           o%   o           
"   
 ,�           �E    1� �   ,� =  	 o%   o           o%   o           � �   ,
"   
 -�           F    1� �   -� =  	 o%   o           o%   o           � �   -
"   
 0�           |F    1� �   0� �   %               o%   o           %               
"   
 0�           �F    1�    0� �   %               o%   o           %               
"   
 �          tG    1� &   � =  	   
"   
 0�           �G    1� :   0� �   %               o%   o           %               
"   
 0�           ,H    1� F   0� 2   %               o%   o           o%   o           
"   
 ,�           �H    1� Z   ,� 2   %               o%   o           o%   o           
"   
 ,�           $I    1� n   ,� �   %               o%   o           o%   o           
"   
 ,�           �I    1� �   ,� 2   %               o%   o           � 7    -
"   
 ,�           J    1� �   ,� �   %               o%   o           %               
"   
 0�           �J    1� �  	 0� �   %               o%   o           %                "    %     start-super-proc �%     adm2/smart.p 7�P �L 
�H T   %              �     }        �GG %              
"   
   �       �K    6� �     
"   
   
�        �K    8
"   
   �        �K    ��     }        �G 4              
"   
 ߱G %              G %              %�   AppService,ASInfo,ASUsePrompt,CacheDuration,CheckCurrentChanged,DestroyStateless,DisconnectAppServer,ServerOperatingMode,ShareData,UpdateFromSource,ForeignFields,ObjectName,OpenOnInit,PromptColumns,PromptOnDelete,RowsToBatch,RebuildOnRepos,ToggleDataTargets  
�H T   %              �     }        �GG %              
"   
 �
"   
 
"   
 �
"   
   (�  L ( l       �        �M    �� �   � P   �        �M    �@    
� @  , 
�       �M    �� �   �p�               �L
�    %              � 8       N    � $         � �          
�    � �   �
"   
 �p� @  , 
�       O    �� �   �p�               �L"  	  , �   � �   -� �   �     }        �A      |    "  	    � �   ,%              (<   \ (    |    �     }        �A� �   �A"  
  -    "  	  �"  
  -  < "  	  �"  
  -(    |    �     }        �A� �   �A"  
  -
�H T   %              �     }        �GG %              
"   
 �
"   
 
"   
 �
"   
   (�  L ( l       �        �P    �� �   � P   �        �P    �@    
� @  , 
�       �P    �� �   �p�               �L
�    %              � 8      Q    � $         � �          
�    � �   �
"   
 �p� @  , 
�       R    �� '  
 �p�               �L"  	  , 
�H T   %              �     }        �GG %              
"   
   
"   
 -
"   
   
"   
   (�  L ( l       �        �R    �� �   � P   �        �R    �@    
� @  , 
�       �R    �� �     p�               �L
�    %              � 8      �R    � $         � �          
�    � �     
"   
 �p� @  , 
�       �S    �� M  
 �p�               �L%     SmartDataObject 
"   
   p� @  , 
�       XT    �� h     p�               �L%               
"   
  p� @  , 
�       �T    �� �    p�               �L%               
"   
  p� @  , 
�       U    �� �    p�               �L(        � 7      � 7      � 7      �     }        �A
�H T   %              �     }        �GG %              
"   
 , (   � 
"   
 �    �        �U    �� �   �
"   
   � 8      DV    � $         � �          
�    � �   �
"   
   �        �V    �
"   
   �       �V    /
"   
   
"   
   �       �V    6� �     
"   
   
�        W    8
"   
   �        4W    �
"   
   �       TW    �
"   
   p�    �    ,
�    �     }        �G 4              
"   
 ߱G %              G %              
�     }        �
"   
    (   � 
"   
 �    �        X    �A"    �A
"   
   
�        dX    �@ � 
"   
 ,"      �       }        �
"   
 %              %                "    %     start-super-proc �%     adm2/appserver.p �,�    � �     
�    �     }        �%               %      Server  - �     }        �    "    0� 7    %                   "    0� 7    %      NONE    p�,  8         $     "    0        � �   �
�    
�H T   %              �     }        �GG %              
"   
 �
"   
 
"   
 �
"   
   (�  L ( l       �        �Z    �� �   � P   �        �Z    �@    
� @  , 
�       �Z    �� �   �p�               �L
�    %              � 8      �Z    � $         � �          
�    � �   �
"   
 �p� @  , 
�       �[    �� N   �p�               �L"    , p�,  8         $     "    0        � �   �
�     "    %     start-super-proc �%     adm2/dataquery.p 6�
�H T   %              �     }        �GG %              
"   
 �
"   
 
"   
 �
"   
 �(�  L ( l       �        ]    �� �   � P   �        ]    �@    
� @  , 
�       $]    �� �   �p�               �L
�    %              � 8      0]    � $         � �   �     
�    � �   �
"   
 �p� @  , 
�       @^    �� 4   �p�               �L%H > 8   dataAvailable,confirmContinue,isUpdatePending,buildDataRequest  
�H T   %              �     }        �GG %              
"   
 �
"   
 
"   
 �
"   
 �(�  L ( l       �        $_    �� �   � P   �        0_    �@    
� @  , 
�       <_    �� �   �p�               �L
�    %              � 8      H_    � $         � �   �     
�    � �   �
"   
 �p� @  , 
�       X`    �� �   �p�               �L%               "    %     start-super-proc �%     adm2/query.p 7�%     start-super-proc �%     adm2/queryext.p % 	    initProps �
�    %d Z T   FOR EACH ttCcbDDocu NO-LOCK,       FIRST Almmmatg OF ttCcbDDocu NO-LOCK INDEXED-REPOSITION  �   � M     � O     � Q     
�     	         �G
"   
 ,�        b    �G
"   
   
"   
    x    (0 4      �        (b    �G%                   �        4b    �GG %              � G     �� H          %              %                   "      %              
"   
       "      �        $c    �
"   
   �        Xc    �
"   
   
�       xc    �"       \      H   "    �((       "      %              � 7      � M   �     
"   
   
"   
  \      H   "      ((       "      %              � 7     � M   ,�        d    �%                   %              %                   "  (    %                  "  (        
"   
 �
"   
 ,0 T       m � "  (  ��        (e    �A @   "      $         � "  (  ,� �   �        4e    �� "  (    
"   
  \ H     H   "      ((       "    �%              � 7    � M     (        "  !  �� 7    ,�        �e    �"  !  �
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
   
"   
   (�  L ( l       �        �f    �� �   � P   �        �f    �@    
� @  , 
�       �f    �� �     p�               �L
�    %              � 8      g    � $         � �          
�    � �     
"   
 �p� @  , 
�       h    �� m   �p�               �L%               
"   
   p� @  , 
�       th    �� �     p�               �L"    , �,  8         $     "    �L        � O   
  
�    
�H T   %              �     }        �GG %              
"   
 $ 
"   
   
"   
 �
"   
 �(�  L ( l       �        Xi    �� �   � P   �        di    �@    
� @  , 
�       pi    �� �   �p�               �L
�    %              � 8      |i    � $         � �   �     
�    � �     
"   
 �p� @  , 
�       �j    �� �   �p�               �L
"   
 , 
"   
   p� @  , 
�       �j    �� �     p�               �L"    , 
"   
  p� @  , 
�       <k    �� ^    p�               �L"    ,     "    ,� 7    %| q l   OPEN QUERY Query-Main FOR EACH ttCcbDDocu NO-LOCK,       FIRST Almmmatg OF ttCcbDDocu NO-LOCK INDEXED-REPOSITION.     "    cb� �    OC((        "    g %                   "    ON� �    ON"    � (   "           "    ON%              @ �,  8         $     "    �        � �     
�    p�,  8         $     � �    -        � �    �
�    %               �    "      � O         %              %                   "      %                  "      "      "     T(        "    -%              "    -� O   "      �       "    ��    "    -� �   � 7      � �   ��    "     � �    S    "      "        "    -%                � @    �     t T     P   4       "      (0       4       -"      � 7      � 7    �� M   -T ,  %              T   "    -"    � O     � �   �� M   -T    �    "    -� �   "      � �   �"      %                   %              %                   "      %                  "      �     "      �     "       \      H   "      ((       "    �%              � 7    � �      4  �     "      
�H T   %              �     }        �GG %              
"   
 �
"   
 
"   
 �
"   
   (�  L ( l       �        �q    �� �   � P   �        �q    �@    
� @  , 
�       �q    �� �   �p�               �L
�    %              � 8      r    � $         � �          
�    � �   �
"   
 �p� @  , 
�       s    �� �  
 �p�               �L"    ,       "  
  -�    � 7    0� O         "  	    �    � �   " � O   0�   � M     � O     � 7    ��   � M     � O   �� �   " 0      "  
  0�    � 7    ,� O         "  	    �    � !   � O   ,   ,        "    �� �    ,�   � M   �� O   ,� 7       ,        "      � �      �   � M   ,� O   � !   0�   � M     � O     � ,!  0   
�H T   %              �     }        �GG %              
"   
 
"   
 �
"   
 
"   
 (�  L ( l       �        �u    �� �   � P   �        �u    �@    
� @  , 
�       �u    �� �   p�               �L
�    %              � 8      �u    � $         � �          
�    � �     
"   
 �p� @  , 
�       �v    �� V   �p�               �L"    , 
"   
   p� @  , 
�       @w    �� "     p�               �L"    , 
"   
  p� @  , 
�       �w    �� �    p�               �L"    ,     %              %                   "      %                  "      �     "      �     "      4 (        "  
    �    � 7      � O         "  	  ��     "    -T    "      "      @ A,    �   � M   � �      "    �"       T      @   "    (        "      � 7    �� 7      � M   �"    ,     "  	   %              D H   @ A,    �   � M   �� �      "    �"    �,    S   "    �� 7    �� O   %                T      @   "    (        "      � 7    �� 7      � M   �"    �     "  
   %                         "    � �      "    �           "      � �    �"      
�H T   %              �     }        �GG %              
"   
 -
"   
   
"   
 -
"   
 �(�  L ( l       �        �{    �� �   � P   �        �{    �@    
� @  , 
�       �{    �� �   -p�               �L
�    %              � 8      �{    � $         � �   �     
�    � �   
"   
 �p� @  , 
�       �|    �� "   �p�               �L"    , 
"   
   p� @  , 
�       @}    �� �     p�               �L"    , "      %               �     }        �%               �     }        �%              %              %              %              %              %              %       	       %       
       %              %              %              %              %              %              %              %              "       "    %     start-super-proc �%     adm2/data.p %     start-super-proc �%     adm2/dataext.p %     start-super-proc �%     adm2/dataextcols.p %     start-super-proc �%     adm2/dataextapi.p ,
�H T   %              �     }        �GG %              
"   
 �
"   
 
"   
 �
"   
 �(�  L ( l       �        ��    �� �   � P   �        ��    �@    
� @  , 
�       ��    �� �   �p�               �L
�    %              � 8      ��    � $         � �   �     
�    � �   �
"   
 �p� @  , 
�       ́    �� :   �p�               �L%               %(     "ccb/dt-nota-cr-db-detail.i" 7�
�H T   %              �     }        �GG %              
"   
 �
"   
 
"   
 �
"   
   (�  L ( l       �        ��    �� �   � P   �        ��    �@    
� @  , 
�       ��    �� �   �p�               �L
�    %              � 8      Ȃ    � $         � �          
�    � �   �
"   
 �p� @  , 
�       ؃    �� *   �p�               �L"    , 
�     	        �G
�H T   %              �     }        �GG %              
"   
 �
"   
 
"   
 �
"   
   (�  L ( l       �        ��    �� �   � P   �        ��    �@    
� @  , 
�       ��    �� �   �p�               �L
�    %              � 8      ��    � $         � �          
�    � �   �
"   
 �p� @  , 
�       ą    �� b  
 �p�               �L
"   
 , 
�     
        �G
�H T   %              �     }        �GG %              
"   
 �
"   
 
"   
 �
"   
   (�  L ( l       �        |�    �� �   � P   �        ��    �@    
� @  , 
�       ��    �� �   �p�               �L
�    %              � 8      ��    � $         � �          
�    � �   �
"   
 �p� @  , 
�       ��    �� /  	 �p�               �L
"   
 , 
"   
      � V"  	   �        �    �
�H T   %              �     }        �GG %              
"   
 �
"   
 
"   
 �
"   
   (�  L ( l       �        ��    �� �   � P   �        ��    �@    
� @  , 
�       ��    �� �   �p�               �L
�    %              � 8      ��    � $         � �          
�    � �   �
"   
 �p� @  , 
�       ��    �� �   �p�               �L"    , 
"   
   �       �    �"      
�     
        �G
�H T   %              �     }        �GG %              
"   
 �
"   
 
"   
 �
"   
   (�  L ( l       �        ��    �� �   � P   �        ��    �@    
� @  , 
�       ��    �� �   �p�               �L
�    %              � 8      Ċ    � $         � �          
�    � �   �
"   
 �p� @  , 
�       ԋ    �� K  	 �p�               �L
"   
 , 
�             �Gp�,  8         $     
"   
 �        � `"   �
�    
�             �Gp�,  8         $     
"   
 �        � r"   �
�    �    � �"     
�        "    �� 7    %     modifyListProperty 
�    %      REMOVE  %     SupportedLinks %     Update-Target  %     bufferValidate  
�    "      �  %      setContextAndInitialize 
�    "      %     bufferCommit    
�    "      "      �    � �"     
�    %               %     bufferCommit    
�    "      "      
�     
        �G�     }        �
�    p�     � T#  	 �% 	    Imp-Total �"       %                    " !     " "                     �           �   l       ��                 h  �  �               �|-                    O   ����    e�          O   ����    R�          O   ����    ��        $  w  �   ���                       �U     
                    � ߱              x  (  �      V      4   ����V                �                      ��                  y  �                  �H-                       y  8  �  �  z  PV            |  �  `      �V      4   �����V                p                      ��                  }  �                  PI-                       }  �  �  o   ~      ,                                 �  �     �V      �  �   �  �V      $  $  �  �  ���                        W     
                    � ߱        8  �   �  @W      L  �   �  `W      `  �   �  �W          $   �  �  ���                       �W  @         �W              � ߱                     T          ,  @   T �            
             
             
             
                 $   4   D          $   4   D   ����        ��                            ����                                            �           �   l       ��                 �  �  �               xJ-                    O   ����    e�          O   ����    R�          O   ����    ��      )                      �          �  $  �    ���                       X     
                    � ߱                  �  �                      ��                   �  �                  �N-                     �  4      4   ����$X      $  �  �  ���                       pX     
                    � ߱        �    �  4  D      �X      4   �����X      /  �  p                               3   �����X  �  �   �  �X          O   �  ��  ��  �X                               , �                          
                               �      ��                            ����                                            �           �   l       ��            	     W  �  �               ��,                    O   ����    e�          O   ����    R�          O   ����    ��        $  w  �   ���                       `a                         � ߱        �  $  x  8  ���                       �a                         � ߱        �a     
                @b  @         b              � ߱        D  $   �  d  ���                         T      �  x                      ��        0         �  �                  �k-      dc     L     �  �      $  �  �  ���                       �b                         � ߱          $  �  �  ���                       �b                         � ߱            4   ����0c  �c     
                �c                     (d                         � ߱          $  �    ���                                                            ��                  �  �                  G-                �     �  �  �  $  �  L  ���                       �d       !       !           � ߱          �      L  �                      ��        0         �  �                  [-     ( $e            �  x      $  �     ���                       �d       (       (           � ߱        �  $  �  x  ���                       �d       (       (           � ߱            4   ����e        �  �  L      @e      4   ����@e                \                      ��                  �  �                  ��                       �  �  �  $  �  �  ���                       �e       !       !           � ߱            O   �  �� ��          $  �  �  ���                       �e                         � ߱        �f     
                g                     `h  @        
  h          �h  @        
 �h          �h                     i     
                �i                     �j  @        
 �j          0k  @        
 �j          �k  @        
 Hk              � ߱        x  V   �  $  ���                        P	    �  �  $	      �k      4   �����k  �k                     4l                     Tl                     �l                         � ߱            $  �  �  ���                       �	    �  l	  |	      �l      4   �����l      �   �  0m      �	  $  �  �	  ���                       pm                         � ߱        �
  $  �  
  ���                       �m                         � ߱          �
                              ��        0         �  �                  ��      n     �     �  @
      $  �  �
  ���                       �m                         � ߱        l  $  �  @  ���                       �m                         � ߱            4   �����m  (n                     pn                     |n                     �n                     �n                         � ߱        D  $  �  |  ���                             �  `  p      o      4   ����o      $  �  �  ���                       4o          `p             � ߱        �  $  �  �  ���                       lp                         � ߱          �      �  \                      ��        0         �  �                  H�       q          �         $  �  �  ���                       �p                         � ߱        L  $  �     ���                       �p                         � ߱            4   �����p      $  �  �  ���                       q                         � ߱        �q     
                r                     `s  @        
  s              � ߱        �  V   �  �  ���                        ls       
       
       �s       	       	       �s                      t                         � ߱        p  $     D  ���                       ,t       
       
       `t       	       	       �t                     �t                         � ߱        �  $  G  �  ���                       �  $  �  �  ���                       <u                         � ߱        hu     
                �u                     4w  @        
 �v          �w  @        
 Lw          �w  @        
 �w              � ߱        (  V   �  �  ���                          8      �                        ��        0    	     4  I                  T��      px     �     4  �      $  4  d  ���                       �w                         � ߱        �  $  4  �  ���                        x                         � ߱        �  4   ����Hx      4   �����x  `  $  9  4  ���                       �x                         � ߱        |    ;  |  �      y      4   ����y                P                      ��                  <  @                  ���                       <  �  Ly                     �y       	       	           � ߱            $  =    ���                             B  �        �y      4   �����y  	              l                      ��             	     D  H                  ���                       D  �  pz                     �z       
       
           � ߱            $  E  $  ���                        {                     4{                         � ߱        �  $  O  �  ���                       h{     
                �{                     4}  @        
 �|          �}  @        
 L}              � ߱            V   ]    ���                                    J           |  D  � X�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
             
                                                                                                                                                                                                                               ) �   �   �   �       (  8  H  X  h  x  �  �  �  �  �  �      (  8  H  X  h  x  �  �  �  �  �  �  �  �      (  8  H  8HXhx��������(8HX     ) �   �   �   �      (  8  H  X  h  x  �  �  �  �  �   �     (  8  H  X  h  x  �  �  �  �  �  �  �  �      (  8  H  8HXhx��������(8HX  �   :                  � �                     �    ��                      ��                            ����                            ]!                          ��                                �   l       ��                      �               y�                    O   ����    e�          O   ����    R�          O   ����    ��              !                    ��                            ����                                            �           �   l       ��                  #  2  �               0�                    O   ����    e�          O   ����    R�          O   ����    ��      �"       �              �                  $                  d  /  /  $     4  ��                      3   ������            T                      3   ������      O   0  ��  ��  ��               �          �  �    �                                             ��                            ����                                            H          �   l       ��                  <  g  �               �                    O   ����    e�          O   ����    R�          O   ����    ��      �"       �              �                $                  �"       ,             �          �"                                �  /  [  t     �  ܍                      3   ������            �                      3   �����     /  ]  �     �  �                      3   ������  x                             3   �����      $   ]  L  ���                                                   � ߱                  �  �                  3   ���� �      $   ]  �  ���                                                   � ߱        X  $  a  ,  ���                       ,�                         � ߱            O   e  ��  ��  H�               �          �  �   @ �                                                              0              0           ��                            ����                                                       �   l       ��                  q  �  �               �|�                    O   ����    e�          O   ����    R�          O   ����    ��      �       $                  �"                    �          �"                      �              /  �  L     \  x�                      3   ����\�  �        |  �                  3   ������      $   �  �  ���                                                   � ߱                                      3   ������      $   �  @  ���                                                   � ߱                     �          �  �   , �                                                                 ��                            ����                                            �           �   l       ��                  �  �  �               �d�                    O   ����    e�          O   ����    R�          O   ����    ��            �  �   �       ��      4   ������      �   �  ��    ��                            ����                                            �           �   l       ��                      �               |G�                    O   ����    e�          O   ����    R�          O   ����    ��      �       !                  M#                       �            �     Ȏ          /     8     H                          3   ����܎            h  x                  3   �����      $     �  ���                                                    � ߱                      $                                                             ��                            ����                                                      �   l       ��                   *  �                (�                    O   ����    e�          O   ����    R�          O   ����    ��      p#   !                   �           �      !                   � ߱        �  $  %  �   ���                             �      D      X    �  h  ��                  &  (  ,              DS�                       &  4      �  ,       ��                            7   ����   "      ��                     �            |                  6   &      " �   ��                    �            |                                                                �  �                                   @            �   �        O   ����  e�          O   ����  R�          O   ����  ��          $  '  p  ���                       �      !                   � ߱                   !  �          �  �    �                                       ! "   ��                             ��                            ����                                8   (  "       8   (  "   TXS appSrvUtils ttCcbDDocu CodCia CodDoc NroDoc NroItm UndVta codmat PreUni PorDto PorDto2 ImpDto ImpLin CanDes Pesmat AftIgv AftIsc PreBas PreVta ImpIgv ImpIsc Factor CanDev CodCli AlmDes Por_Dsctos Flg_Factor CodDiv FchDoc ImpCto puntos mrguti ImpPro ImpDto2 PorDcto_Adelanto ImpDcto_Adelanto Dcto_Otros_Mot Dcto_Otros_Factor Dcto_Otros_VV Dcto_Otros_PV cTipoAfectacion cPreUniSinImpuesto FactorDescuento TasaIGV ImporteUnitarioSinImpuesto ImporteReferencial ImporteBaseDescuento ImporteDescuento ImporteTotalSinImpuesto MontoBaseIGV ImporteIGV ImporteTotalImpuestos ImporteUnitarioConImpuesto cImporteVentaExonerado cImporteVentaGratuito cSumaImpteTotalSinImpuesto cMontoBaseIGV cSumaIGV cOtrosTributosOpGratuito ImpuestoBolsaPlastico MontoTributoBolsaPlastico CantidadBolsaPlastico MontoUnitarioBolsaPlastico cImporteTotalConImpuesto ImporteBaseDescuentoNoAfecto FactorDescuentoNoAfecto ImporteDescuentoNoAfecto Almmmatg Cat�logo de Materiales D:\newsie\on_in_co\aplic\ccb\dt-nota-cr-db-detail.w should only be RUN PERSISTENT. ADDROW CANCELROW CANNAVIGATE CLOSEQUERY COLUMNPROPS COLVALUES COPYROW CREATEROW DELETEROW FETCHROW FETCHROWIDENT FINDROW FINDROWWHERE FIRSTROWIDS GETLASTCOMMITERRORTYPE HASFOREIGNKEYCHANGED OPENDATAQUERY OPENQUERY PREPAREQUERY ROWAVAILABLE ROWVALUES SUBMITROW UPDATEROW GETOBJECTTYPE xiRocketIndexLimit ADDQUERYWHERE ASSIGNQUERYSELECTION BUFFERCOMPAREDBTORO BUFFERWHERECLAUSE COLUMNDATATYPE COLUMNDBCOLUMN COLUMNQUERYSELECTION COLUMNTABLE COLUMNVALMSG DBCOLUMNDATANAME DBCOLUMNHANDLE EXCLUDECOLUMNS GETDATACOLUMNS GETFOREIGNVALUES GETQUERYPOSITION GETQUERYSORT GETQUERYSTRING GETQUERYWHERE GETTARGETPROCEDURE INDEXINFORMATION INSERTEXPRESSION NEWQUERYSTRING NEWQUERYVALIDATE NEWQUERYWHERE NEWWHERECLAUSE REFRESHROWIDENT REMOVEFOREIGNKEY REMOVEQUERYSELECTION RESOLVEBUFFER ROWIDWHERE ROWIDWHERECOLS SETQUERYPOSITION SETQUERYSORT SETQUERYSTRING SETQUERYWHERE WHERECLAUSEBUFFER GETAPPSERVICE GETASBOUND GETASDIVISION GETASHANDLE GETASHASSTARTED GETASINFO GETASINITIALIZEONRUN GETASUSEPROMPT GETSERVERFILENAME GETSERVEROPERATINGMODE RUNSERVERPROCEDURE SETAPPSERVICE SETASDIVISION SETASHANDLE SETASINFO SETASINITIALIZEONRUN SETASUSEPROMPT SETSERVERFILENAME SETSERVEROPERATINGMODE gshAstraAppserver gshSessionManager gshRIManager gshSecurityManager gshProfileManager gshRepositoryManager gshTranslationManager gshWebManager gscSessionId gsdSessionObj gshFinManager gshGenManager gshAgnManager gsdTempUniqueID gsdUserObj gsdRenderTypeObj gsdSessionScopeObj ghProp ghADMProps ghADMPropsBuf glADMLoadFromRepos glADMOk ANYMESSAGE ASSIGNLINKPROPERTY FETCHMESSAGES GETCHILDDATAKEY GETCONTAINERHANDLE GETCONTAINERHIDDEN GETCONTAINERSOURCE GETCONTAINERSOURCEEVENTS GETCONTAINERTYPE GETDATALINKSENABLED GETDATASOURCE GETDATASOURCEEVENTS GETDATASOURCENAMES GETDATATARGET GETDATATARGETEVENTS GETDBAWARE GETDESIGNDATAOBJECT GETDYNAMICOBJECT GETINSTANCEPROPERTIES GETLOGICALOBJECTNAME GETLOGICALVERSION GETOBJECTHIDDEN GETOBJECTINITIALIZED GETOBJECTNAME GETOBJECTPAGE GETOBJECTPARENT GETOBJECTVERSION GETOBJECTVERSIONNUMBER GETPARENTDATAKEY GETPASSTHROUGHLINKS GETPHYSICALOBJECTNAME GETPHYSICALVERSION GETPROPERTYDIALOG GETQUERYOBJECT GETRUNATTRIBUTE GETSUPPORTEDLINKS GETTRANSLATABLEPROPERTIES GETUIBMODE GETUSERPROPERTY INSTANCEPROPERTYLIST LINKHANDLES LINKPROPERTY MAPPEDENTRY MESSAGENUMBER PROPERTYTYPE REVIEWMESSAGES SETCHILDDATAKEY SETCONTAINERHIDDEN SETCONTAINERSOURCE SETCONTAINERSOURCEEVENTS SETDATALINKSENABLED SETDATASOURCE SETDATASOURCEEVENTS SETDATASOURCENAMES SETDATATARGET SETDATATARGETEVENTS SETDBAWARE SETDESIGNDATAOBJECT SETDYNAMICOBJECT SETINSTANCEPROPERTIES SETLOGICALOBJECTNAME SETLOGICALVERSION SETOBJECTNAME SETOBJECTPARENT SETOBJECTVERSION SETPARENTDATAKEY SETPASSTHROUGHLINKS SETPHYSICALOBJECTNAME SETPHYSICALVERSION SETRUNATTRIBUTE SETSUPPORTEDLINKS SETTRANSLATABLEPROPERTIES SETUIBMODE SETUSERPROPERTY SHOWMESSAGE SIGNATURE prepareInstance ObjectName CHAR  ObjectVersion ADM2.2 ObjectType SmartDataObject ContainerType PropertyDialog adm2/support/datad.w QueryObject LOGICAL ContainerHandle HANDLE InstanceProperties AppService,ASInfo,ASUsePrompt,CacheDuration,CheckCurrentChanged,DestroyStateless,DisconnectAppServer,ServerOperatingMode,ShareData,UpdateFromSource,ForeignFields,ObjectName,OpenOnInit,PromptColumns,PromptOnDelete,RowsToBatch,RebuildOnRepos,ToggleDataTargets SupportedLinks Data-Source,Data-Target,Navigation-Target,Update-Target,Commit-Target,Filter-Target ContainerHidden ObjectInitialized ObjectHidden ObjectsCreated HideOnInit UIBMode ContainerSource ContainerSourceEvents initializeObject,hideObject,viewObject,destroyObject,enableObject,confirmExit,confirmCancel,confirmOk,isUpdateActive DataSource DataSourceEvents dataAvailable,queryPosition,updateState,deleteComplete,fetchDataSet,confirmContinue,confirmCommit,confirmUndo,assignMaxGuess,isUpdatePending TranslatableProperties ObjectPage INT DBAware DesignDataObject DataSourceNames DataTarget DataTargetEvents CHARACTER updateState,rowObjectState,fetchBatch,LinkState LogicalObjectName PhysicalObjectName LogicalVersion PhysicalVersion DynamicObject RunAttribute ChildDataKey ParentDataKey DataLinksEnabled InactiveLinks InstanceId DECIMAL SuperProcedure SuperProcedureMode SuperProcedureHandle LayoutPosition ClassName RenderingProcedure ThinRenderingProcedure Label cType ADMProps Target WHERE Target = WIDGET-H(" ") AppService ASDivision ASHandle ASHasConnected ASHasStarted ASInfo ASInitializeOnRun ASUsePrompt BindSignature BindScope ServerOperatingMode ServerFileName ServerFirstCall NeedContext AutoCommit BLOBColumns BufferHandles CLOBColumns CommitSource CommitSourceEvents commitTransaction,undoTransaction CommitTarget CommitTargetEvents rowObjectState CurrentRowid ROWID CurrentUpdateSource DataColumns DataHandle DataIsFetched DataModified DataQueryBrowsed DataQueryString FetchOnOpen FillBatchOnRepos FilterActive FilterAvailable FilterSource FilterWindow FirstRowNum ForeignFields ForeignValues IgnoreTreeViewFilter IndexInformation LargeColumns LastRowNum NavigationSource NavigationSourceEvents fetchFirst,fetchNext,fetchPrev,fetchLast,startFilter OpenOnInit PrimarySDOSource PromptColumns PromptOnDelete QueryColumns QueryPosition QueryString RebuildOnRepos RowObject RowObjectState NoUpdates RowsToBatch Tables ToggleDataTargets TransferChildrenForAll UpdatableColumns UpdatableWhenNew UpdateSource AssignList AuditEnabled BaseQuery CalcFieldList CheckLastOnOpen DataColumnsByTable DBNames EntityFields FetchHasAudit FetchHasComment FetchAutoComment FirstResultRow KeyFields KeyTableId LastDBRowIdent LastResultRow NewBatchInfo NoLockReadOnlyTables PhysicalTables PositionForClient QueryHandle QueryRowIdent RequiredProperties SkipTransferDBRow TempTables UpdatableColumnsByTable UpdateFromSource RowObjUpd RowObjectTable RowObjUpdTable CheckCurrentChanged StatelessSavedProperties CheckCurrentChanged,RowObjectState,LastResultRow,FirstResultRow,QueryRowIdent DestroyStateless DisconnectAppServer ServerSubmitValidation DataFieldDefs "ccb/dt-nota-cr-db-detail.i" QueryContainer QueryContext AsynchronousSDO DataLogicProcedure DataLogicObject DataReadHandler DataReadColumns DataReadBuffer DataDelimiter | DataReadFormat TrimNumeric IsRowObjectExternal IsRowObjUpdExternal ManualAddQueryWhere DynamicData LastCommitErrorType LastCommitErrorKeys RunDataLogicProxy SchemaLocation CacheDuration INTEGER ShareData ghContainer adm2/smart.p cObjectName iStart / \ . hReposBuffer hPropTable hBuffer hTable deleteProperties ADM-CLONE-PROPS pcProcName hProc START-SUPER-PROC cAppService cASDivision cServerOperatingMode adm2/appserver.p getAppService Server NONE setASDivision setAppService adm2/dataquery.p dataAvailable,confirmContinue,isUpdatePending,buildDataRequest adm2/query.p adm2/queryext.p cTable iTable cColumns cDataCols cUpdCols cCalcData cCalcUpd iNumData iNumUpd cBuffers cKeyFields cAssignList iAssigns iPos iEntry iCount cTables cTableAssign cDbEntry cField cKeyTable cQueryString FOR EACH ttCcbDDocu NO-LOCK,       FIRST Almmmatg OF ttCcbDDocu NO-LOCK INDEXED-REPOSITION ,   ttCcbDDocu Almmmatg hQuery cOpenQuery cDBNames cPhysicalTables cKeyTableEntityFields cKeyTableEntityValues cKeyTableEntityMnemonic cKeyTableEntityObjField cDBName cEntityFields lHasObjectField lHasAudit lHasComment lHasAutoComment iLookup iAlias  STATIC setDBNames OPEN QUERY Query-Main FOR EACH ttCcbDDocu NO-LOCK,       FIRST Almmmatg OF ttCcbDDocu NO-LOCK INDEXED-REPOSITION.  FOR   PRESELECT  setOpenQuery 5 showMessage ; NroItm codmat UndVta CanDes ImpLin DesMat DesMar NroItm codmat DesMat DesMar UndVta CanDes ImpLin Query-Main INITPROPS piTableIndex lRet DELETERECORDSTATIC adm2/data.p adm2/dataext.p adm2/dataextcols.p adm2/dataextapi.p DesMat DesMar RowNum RowIdent RowMod RowIdentIdx RowUserProp ChangedFields cContainerType hRowObject hDataQuery cDataFieldDefs FOR EACH  setRowObjectTable setRowObjUpdTable getUpdatableColumns REMOVE Update-Target PUSHROWOBJUPDTABLE pcValType PUSHTABLEANDVALIDATE pcContext pcMessages pcUndoIds obtainContextForClient REMOTECOMMIT SERVERCOMMIT GETROWOBJUPDSTATIC DISABLE_UI pTotal openQuery EXPORT-TEMP-TABLE F-ImpTot B-RowObject IMP-TOTAL llave01 llave02 llave03 llave04 qDataQuery 4  �8  d  0G      / �<   ��      0         pcServices      ��      T         pcServices  �   ��      x         piStartRow  �   ��      �         piStartRow  �   ��      �         piStartRow  �   ��      �         piStartRow      ��              piStartRow  <  ��      ,        pcViewColList       ��      T       
 pcViewColList       ��      |        pcRelative  �  ��      �        pcSdoName       ��      �  �     
 pcSdoName       ��      �        plForwards      ��              pcContext       ��      0        plUpdate    `  ��      T        pcFieldList �  ��      x        pcFieldList     ��      �        pcFieldList �  ��      �        piocContext �  ��      �        piocContext   ��              piocContext 8  ��      ,        piocContext \  ��      P        piocContext �  ��      t        piocContext �  ��      �  �     
 piocContext     ��      �        piocContext     ��      �        pcState     ��               pcContext   0  ��      $        piStartRow  T  ��      H        piStartRow  x  ��      l        piStartRow  �  ��      �        piStartRow  �  ��      �        piStartRow      ��      �  �     
 piStartRow      ��      �  �     
 phRowObjUpd     ��               pcProperties    T  ��      H        piStartRow  x  ��      l        piStartRow  �  ��      �        piStartRow  �  ��      �        piStartRow  �  ��      �        piStartRow      ��      �  �     
 piStartRow  ,  ��               pcRowIdent      ��      D        pcRowIdent  t  ��      h        pcRowIdent  �  ��      �        pcRowIdent      ��      �        pcRowIdent  �  ��      �        pcValueList     ��      �        pcValueList 4  ��              pcPropertiesForServer       ��      L        pcPropertiesForServer   �  ��      |        pcFieldList �  ��      �        pcFieldList �  ��      �        pcFieldList     ��      �        pcFieldList   ��              pcWhere     ��      ,        pcWhere     ��      L        pcState     ��      l       
 phRowObjUpd �  ��      �       
 phRowObj    �  ��      �       
 phRowObj    �  ��      �        phRowObj        ��      �        phRowObj        ��       	        pioCancel       ��      D	        pcRelative      ��      h	       
 phFilterContainer       ��      �	       
 phRowObjUpd �	  ��      �	        pcRowIdent      ��      �	        pcRowIdent      ��       
       
 phAppService        ��      (
        pcMode  T
  ��      H
       
 phSource    x
  ��      l
        phSource        ��      �
       
 phSource    �
  ��      �
        pcText  �
  ��      �
        pcText      ��      �
        pcText     ��             
 phObject    D  ��      8       
 phObject        ��      \        phObject        ��      �        pcField     ��      �        pcCursor    �  ��      �       
 phCaller    �  ��      �        phCaller      ��              phCaller        ��      0        phCaller    \  ��      T        pcMod   |  ��      t        pcMod       ��      �       
 pcMod   �  ��      �       
 phSource    �  ��      �        phSource        ��      �       
 phSource    (  ��               pdRow       ��      @        pdRow       ��      `       
 hTarget �  ��      �        pcMessage       ��      �        pcMessage       ��      �        plEnabled             �     cType       0     V   �                             getObjectType   Z	  r	  t	  `        P  
   hReposBuffer    �        t  
   hPropTable  �        �  
   hBuffer           �  
   hTable  �  �     W   <          �                  adm-clone-props w  x  y  z  |  }  ~    �  �  �  �  �  �  �  �  �            P  
   hProc             p        pcProcName  �  �  	   X   <  X      �                  start-super-proc    �  �  �  �  �  �  �  �  �           �     cTable               iTable  <        0     cColumns    \        P     cDataCols   |        p     cUpdCols    �        �     cCalcData   �        �     cCalcUpd    �     	   �     iNumData    �     
   �     iNumUpd              cBuffers    8        ,     cKeyFields  X        L     cAssignList x        l     iAssigns    �        �     iPos    �        �     iEntry  �        �     iCount  �        �     cTables        �     cTableAssign    ,              cDbEntry    H        @     cField  h        \     cKeyTable   �        |     cQueryString    �        �  
   hQuery  �        �  
   hBuffer �        �     cOpenQuery          �     cDBNames    (             cPhysicalTables T        <     cKeyTableEntityFields   �        h     cKeyTableEntityValues   �        �     cKeyTableEntityMnemonic �         �     cKeyTableEntityObjField �     !   �     cDBName      "        cEntityFields   <     #   ,     lHasObjectField \     $   P     lHasAudit   |     %   p     lHasComment �     &   �     lHasAutoComment �     '   �     iLookup        (   �     iAlias  |    4   Y   �                            initProps   w  x  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �     G  �  �  4  9  ;  <  =  @  B  D  E  H  I  O  ]  �            �     lRet                      piTableIndex    �  l  ,   Z   �         X                  deleteRecordStatic  �  �  �  �  �  �  �  �  �  �      +  ,  H  I  e  f  �  �  �  �  �  �  �  �  �  �      0  1  M  N  j  k  �  �  �  �  �  �  �  �                 !       (  x     [             d                  pushRowObjUpdTable    �        �        pcValType                  $       4        \       |      �                  pushTableAndValidate    /  0  2  0        $        pcContext   H             $       l        `        pcMessages            �        pcUndoIds   �  �     ]             �                  remoteCommit    [  ]  a  e  g  �             $                        pcMessages            8        pcUndoIds   �  �     ^       �      t                  serverCommit    �  �  D  �     _               �                  getRowObjUpdStatic  �  �  �       `                                 disable_UI  �  �  4             !                  L        pTotal  �  �     a             �                  Export-Temp-Table             !      �        F-ImpTot         " B  �  B-RowObject T        b       �  �                    Imp-Total   %  &  '  (  *  �   *  
     �$      �)                      \"  t  �  A   ttCcbDDocu  �         �         �         �         �         �         �         �         �         �         �         �         �         �         �                                            $         ,         4         <         D        P         \         d         l         t         |         �         �         �        �        �         �         �         �                              $          4          <          X          l          �          �          �          �          �          �           !         !         0!         L!         \!         h!         �!         �!         �!         �!         �!         "         ("         @"         CodCia  CodDoc  NroDoc  NroItm  UndVta  codmat  PreUni  PorDto  ImpDto  ImpLin  CanDes  AftIgv  AftIsc  PreBas  PreVta  ImpIgv  ImpIsc  Factor  CanDev  PorDto2 Pesmat  CodCli  AlmDes  Por_Dsctos  Flg_Factor  FchDoc  CodDiv  ImpCto  puntos  mrguti  ImpPro  ImpDto2 PorDcto_Adelanto    ImpDcto_Adelanto    Dcto_Otros_Mot  Dcto_Otros_Factor   Dcto_Otros_VV   Dcto_Otros_PV   cTipoAfectacion cPreUniSinImpuesto  FactorDescuento TasaIGV ImporteUnitarioSinImpuesto  ImporteReferencial  ImporteBaseDescuento    ImporteDescuento    ImporteTotalSinImpuesto MontoBaseIGV    ImporteIGV  ImporteTotalImpuestos   ImporteUnitarioConImpuesto  cImporteVentaExonerado  cImporteVentaGratuito   cSumaImpteTotalSinImpuesto  cMontoBaseIGV   cSumaIGV    cOtrosTributosOpGratuito    ImpuestoBolsaPlastico   MontoTributoBolsaPlastico   CantidadBolsaPlastico   MontoUnitarioBolsaPlastico  cImporteTotalConImpuesto    ImporteBaseDescuentoNoAfecto    FactorDescuentoNoAfecto ImporteDescuentoNoAfecto    t#  l"  x"     RowObject   #         #         #          #         (#         0#         8#         @#         H#         T#         \#         h#         NroItm  codmat  DesMat  DesMar  UndVta  CanDes  ImpLin  RowNum  RowIdent    RowMod  RowIdentIdx RowUserProp     �#  �#     RowObjUpd   ,$         4$         <$         D$         L$         T$         \$         d$         l$         x$         �$         �$         �$         NroItm  codmat  DesMat  DesMar  UndVta  CanDes  ImpLin  RowNum  RowIdent    RowMod  RowIdentIdx RowUserProp ChangedFields   �$          �$  
   appSrvUtils �$       �$     xiRocketIndexLimit  %        %  
   gshAstraAppserver   @%        ,%  
   gshSessionManager   d%        T%  
   gshRIManager    �%        x%  
   gshSecurityManager  �%  	 	     �%  
   gshProfileManager   �%  
 
     �%  
   gshRepositoryManager    &        �%  
   gshTranslationManager   0&         &  
   gshWebManager   T&        D&     gscSessionId    x&        h&     gsdSessionObj   �&        �&  
   gshFinManager   �&        �&  
   gshGenManager   �&        �&  
   gshAgnManager   '        �&     gsdTempUniqueID ('        '     gsdUserObj  P'        <'     gsdRenderTypeObj    x'        d'     gsdSessionScopeObj  �'       �'  
   ghProp  �'       �'  
   ghADMProps  �'       �'  
   ghADMPropsBuf    (       �'     glADMLoadFromRepos  (       (     glADMOk <(       0(  
   ghContainer \(    	   P(     cObjectName x(    
   p(     iStart  �(       �(     cAppService �(       �(     cASDivision �(       �(     cServerOperatingMode    )       �(     cContainerType  ,)       )     cQueryString    L)       @)  
   hRowObject  l)       `)  
   hDataQuery  �)       �)     cColumns             �)     cDataFieldDefs  �)    X  �)  ttCcbDDocu  �)       �)  Almmmatg    *    H  �)  RowObject         X  *  RowObjUpd            @   �   �   �   �   (  )  *  +  B  N  O  P  R  T  U  V  Z  [  ^  _  `  a  c  e  g  i  j  k  n  p  q  s  t  u  v  w  }    �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  
  K
  L
  N
  O
  P
  Q
  R
  S
  U
  V
  W
  X
  Y
  Z
  [
  \
  ]
  ^
  _
  `
  a
  b
  c
  d
  e
  f
  g
  h
  i
  j
  k
  l
  m
  o
  p
  q
  r
  s
  t
  u
  v
  w
  x
  y
  z
  {
  |
  }
  ~
  
  �
  �
  �
  �
  �
  �
                     	  
                            �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  7  �  �  �  �  �  �  �  �      (  :  Y  [  p  �      +  ;  <  =  @  A  B  I  J  g  {  �  -  .  :  ^  �  �  �  �  �  K  �  �  �  �  �  �  �  @  Z  d  ~  �  �  �  �  �  �  �  �    *  4  N  p  {  |      ��  C:\Progress\OpenEdge\src\adm2\data.i H.  �) . %C:\Progress\OpenEdge\src\adm2\custom\datacustom.i    x.  �� - C:\Progress\OpenEdge\src\adm2\robjflds.i �.  -� , D:\newsie\on_in_co\aplic\ccb\dt-nota-cr-db-detail.i  �.  �  C:\Progress\OpenEdge\src\adm2\query.i    ,/  z + C:\Progress\OpenEdge\src\adm2\delrecst.i `/  `W * C:\Progress\OpenEdge\src\adm2\tblprep.i  �/  F� ) C:\Progress\OpenEdge\gui\fnarg   �/   ( %C:\Progress\OpenEdge\src\adm2\custom\querycustom.i   �/  �   C:\Progress\OpenEdge\src\adm2\dataquery.i    40  �Z ' %C:\Progress\OpenEdge\src\adm2\custom\dataquerycustom.i   l0  �< ! C:\Progress\OpenEdge\src\adm2\appserver.i    �0  �� & %C:\Progress\OpenEdge\src\adm2\custom\appservercustom.i   �0  I� " C:\Progress\OpenEdge\src\adm2\smart.i    ,1  Ds % C:\Progress\OpenEdge\gui\fn  `1  tw $ %C:\Progress\OpenEdge\src\adm2\custom\smartcustom.i   �1  Q. # C:\Progress\OpenEdge\gui\set �1  �>  C:\Progress\OpenEdge\src\adm2\dataprop.i �1  ��  %C:\Progress\OpenEdge\src\adm2\custom\datapropcustom.i    $2  ��  %C:\Progress\OpenEdge\src\adm2\custom\dataprtocustom.i    h2  �K  C:\Progress\OpenEdge\src\adm2\qryprop.i  �2  -�  %C:\Progress\OpenEdge\src\adm2\custom\qrypropcustom.i �2  ��  %C:\Progress\OpenEdge\src\adm2\custom\qryprtocustom.i  3   	 C:\Progress\OpenEdge\src\adm2\dataqueryprop.i    `3  �d  %C:\Progress\OpenEdge\src\adm2\custom\dataquerypropcustom.i   �3  ��  %C:\Progress\OpenEdge\src\adm2\custom\dataqueryprtocustom.i   �3  �l  C:\Progress\OpenEdge\src\adm2\appsprop.i ,4  ɏ  %C:\Progress\OpenEdge\src\adm2\custom\appspropcustom.i    `4  V  %C:\Progress\OpenEdge\src\adm2\custom\appsprtocustom.i    �4  i$  C:\Progress\OpenEdge\src\adm2\smrtprop.i �4  �j  C:\Progress\OpenEdge\gui\get 5  �  %C:\Progress\OpenEdge\src\adm2\custom\smrtpropcustom.i    D5  ��  %C:\Progress\OpenEdge\src\adm2\custom\smrtprtocustom.i    �5  ��  C:\Progress\OpenEdge\src\adm2\smrtprto.i �5  Su  C:\Progress\OpenEdge\src\adm2\globals.i   6  M�  %C:\Progress\OpenEdge\src\adm2\custom\globalscustom.i 46  )a  %C:\Progress\OpenEdge\src\adm2\custom\smartdefscustom.i   t6  �  C:\Progress\OpenEdge\src\adm2\appsprto.i �6  ��  %C:\Progress\OpenEdge\src\adm2\custom\appserverdefscustom.i   �6  ��  C:\Progress\OpenEdge\src\adm2\dataqueryprto.i    47  ª 
 %C:\Progress\OpenEdge\src\adm2\custom\dataquerydefscustom.i   p7  ��  C:\Progress\OpenEdge\src\adm2\qryprto.i  �7  �  %C:\Progress\OpenEdge\src\adm2\custom\querydefscustom.i   �7  �`  C:\Progress\OpenEdge\src\adm2\dataprto.i 08  �  %C:\Progress\OpenEdge\src\adm2\custom\datadefscustom.i    d8  e�  !C:\Progress\OpenEdge\gui\adecomm\appserv.i   �8  �    D:\newsie\on_in_co\aplic\ccb\dt-nota-cr-db-detail.w      �   �       9  [  d     09     b  %   @9  �   �     P9     �  .   `9  �   z     p9     [     �9  �   X     �9     6  #   �9  �   4     �9       #   �9  �        �9     �  #   �9  �   �     �9     �  #    :  �   �     :     �  #    :  �   �     0:     �  #   @:  �   ~     P:     \  #   `:  �   Z     p:     8  #   �:  �   +     �:       -   �:  �        �:       ,   �:  k   �     �:  �  �     �:     �  +   �:  �  �      ;     �  +   ;  �  �      ;     m  +   0;  �  j     @;     P  +   P;  �  M     `;     3  +   p;  �  0     �;       +   �;  �       �;     �  +   �;  �  �     �;     �  +   �;  �  �     �;     �  +   �;  �  �      <     �  +   <  �  �      <     �  +   0<  �  �     @<     h  +   P<  �  e     `<     K  +   p<  �  H     �<     .  +   �<  �  +     �<       +   �<  �       �<     �  +   �<  �  �     �<     �  +   �<  �  �      =     �  +   =  �  �      =     x  #   0=  �  w     @=     U  #   P=  j  0     `=       #   p=  i       �=     �  #   �=  h  �     �=     �  #   �=  ^  �     �=     �  *   �=  ]  �     �=     q  *   �=  \  p      >     J  *   >  [  I      >     #  *   0>  Z  "     @>     �  *   P>  Y  �     `>     �  *   p>  X  �     �>     �  *   �>  W  �     �>     �  *   �>  V  �     �>     `  *   �>  U  _     �>     9  *   �>  T  8      ?       *   ?  S        ?     �  *   0?  R  �     @?     �  *   P?  Q  �     `?     �  *   p?  P  �     �?     v  *   �?  O  u     �?     O  *   �?  N  N     �?     (  *   �?  M  '     �?       *   �?  ?  �      @     �  #   @  	  �      @     �  )   0@  �   �     @@     f  #   P@  �   e     `@     C  #   p@  �   B     �@        #   �@  �        �@     �  #   �@  �   �     �@     �  #   �@  �   �     �@     �  #   �@  �   G      A     �  (   A  g   �      A  a   �      0A     z  '   @A  _   x      PA     V  #   `A  ]   T      pA     2  #   �A  I         �A  �     !   �A     �  &   �A  �   �  !   �A     �  #   �A  �   �  !   �A     s  #   �A  �   q  !    B     O  #   B  g   5  !    B          0B  O   �  !   @B  �   �  "   PB     �  %   `B  �   V  "   pB     �  $   �B  �   �  "   �B     �  #   �B  �   �  "   �B     �  #   �B  �   �  "   �B     �  #   �B  �   �  "   �B     h  #    C  �   T  "   C     2  #    C  }   &  "   0C       #   @C     �  "   PC     :  !   `C     �      pC     �     �C     @     �C  �   7     �C  O   )     �C          �C     �     �C  �   �     �C  �   �     �C  O   z      D     i     D           D  y   �
     0D  �   �
  	   @D  G   �
     PD     �
     `D     �
     pD  c   #
  	   �D  x   
     �D  M   
     �D     �	     �D     �	     �D  a   �	     �D  �  q	     �D     R	     �D  �  	      E  O   	     E      	      E     �     0E  �   �     @E     �     PE          `E  x   �     pE     �     �E     m     �E     i     �E     U     �E     <     �E  Q   ,     �E     �     �E     �     �E     �      F     l     F  ]   f  	    F     \     0F       	   @F       
   PF     �  	   `F  Z   �     pF     �     �F     �     �F     �     �F     �     �F  c   p     �F     N     �F          �F     �      �F     �       G     �      G     !        G           