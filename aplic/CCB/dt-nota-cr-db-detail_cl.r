	��V��hf<7   �                                              �: 373C00EFutf-8 MAIN D:\newsie\on_in_co\aplic\ccb\dt-nota-cr-db-detail_cl.w,, PROCEDURE disable_UI,, PROCEDURE serverCommit,,INPUT-OUTPUT RowObjUpd TABLE,OUTPUT pcMessages CHARACTER,OUTPUT pcUndoIds CHARACTER PROCEDURE remoteCommit,,INPUT-OUTPUT pcContext CHARACTER,INPUT-OUTPUT RowObjUpd TABLE,OUTPUT pcMessages CHARACTER,OUTPUT pcUndoIds CHARACTER PROCEDURE pushTableAndValidate,,INPUT pcValType CHARACTER,INPUT-OUTPUT RowObjUpd TABLE PROCEDURE pushRowObjUpdTable,,INPUT RowObjUpd TABLE PROCEDURE initProps,, PROCEDURE start-super-proc,,INPUT pcProcName CHARACTER PROCEDURE adm-clone-props,, PROCEDURE viewObject,, PROCEDURE toggleData,,INPUT plEnabled LOGICAL PROCEDURE showMessageProcedure,,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL PROCEDURE returnFocus,,INPUT hTarget HANDLE PROCEDURE repositionObject,,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL PROCEDURE removeLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE removeAllLinks,, PROCEDURE modifyUserLinks,,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE PROCEDURE modifyListProperty,,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER PROCEDURE hideObject,, PROCEDURE exitObject,, PROCEDURE editInstanceProperties,, PROCEDURE displayLinks,, PROCEDURE createControls,, PROCEDURE changeCursor,,INPUT pcCursor CHARACTER PROCEDURE applyEntry,,INPUT pcField CHARACTER PROCEDURE adjustTabOrder,,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER PROCEDURE addMessage,,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER PROCEDURE addLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE unbindServer,,INPUT pcMode CHARACTER PROCEDURE runServerObject,,INPUT phAppService HANDLE PROCEDURE disconnectObject,, PROCEDURE destroyObject,, PROCEDURE bindServer,, PROCEDURE transferDBRow,,INPUT pcRowIdent CHARACTER,INPUT piRowNum INTEGER PROCEDURE startFilter,, PROCEDURE releaseDBRow,, PROCEDURE refetchDBRow,,INPUT phRowObjUpd HANDLE PROCEDURE filterContainerHandler,,INPUT phFilterContainer HANDLE PROCEDURE fetchDBRowForUpdate,, PROCEDURE confirmContinue,,INPUT-OUTPUT pioCancel LOGICAL PROCEDURE compareDBRow,, PROCEDURE bufferCopyDBToRO,,INPUT phRowObj HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER PROCEDURE assignDBRow,,INPUT phRowObjUpd HANDLE PROCEDURE updateState,,INPUT pcState CHARACTER PROCEDURE updateQueryPosition,, PROCEDURE updateAddQueryWhere,,INPUT pcWhere CHARACTER,INPUT pcField CHARACTER PROCEDURE undoTransaction,, PROCEDURE transferToExcel,,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT plUseExisting LOGICAL,INPUT piMaxRecords INTEGER PROCEDURE synchronizeProperties,,INPUT pcPropertiesForServer CHARACTER,OUTPUT pcPropertiesForClient CHARACTER PROCEDURE submitValidation,,INPUT pcValueList CHARACTER,INPUT pcUpdColumns CHARACTER PROCEDURE submitForeignKey,,INPUT pcRowIdent CHARACTER,INPUT-OUTPUT pcValueList CHARACTER,INPUT-OUTPUT pcUpdColumns CHARACTER PROCEDURE submitCommit,,INPUT pcRowIdent CHARACTER,INPUT plReopen LOGICAL PROCEDURE startServerObject,, PROCEDURE setPropertyList,,INPUT pcProperties CHARACTER PROCEDURE serverFetchRowObjUpdTable,,OUTPUT phRowObjUpd TABLE-HANDLE PROCEDURE serverSendRows,,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER,OUTPUT phRowObject TABLE-HANDLE PROCEDURE saveContextAndDestroy,,OUTPUT pcContext CHARACTER PROCEDURE rowObjectState,,INPUT pcState CHARACTER PROCEDURE retrieveFilter,, PROCEDURE restartServerObject,, PROCEDURE remoteSendRows,,INPUT-OUTPUT piocContext CHARACTER,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT pioRowsReturned INTEGER,OUTPUT phRowObject TABLE-HANDLE,OUTPUT pocMessages CHARACTER PROCEDURE refreshRow,, PROCEDURE printToCrystal,,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT piMaxRecords INTEGER PROCEDURE isUpdatePending,,INPUT-OUTPUT plUpdate LOGICAL PROCEDURE initializeServerObject,, PROCEDURE initializeObject,, PROCEDURE home,, PROCEDURE genContextList,,OUTPUT pcContext CHARACTER PROCEDURE fetchPrev,, PROCEDURE fetchNext,, PROCEDURE fetchLast,, PROCEDURE fetchFirst,, PROCEDURE fetchBatch,,INPUT plForwards LOGICAL PROCEDURE endClientDataRequest,, PROCEDURE destroyServerObject,, PROCEDURE describeSchema,,INPUT pcSdoName CHARACTER,OUTPUT hTtSchema TABLE-HANDLE PROCEDURE dataAvailable,,INPUT pcRelative CHARACTER PROCEDURE copyColumns,,INPUT pcViewColList CHARACTER,INPUT phDataQuery HANDLE PROCEDURE commitTransaction,, PROCEDURE clientSendRows,,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER PROCEDURE batchServices,,INPUT pcServices CHARACTER,OUTPUT pcValues CHARACTER FUNCTION deleteRecordStatic,logical,INPUT piTableIndex INTEGER FUNCTION getRowObjUpdStatic,widget-handle, FUNCTION Signature,CHARACTER,INPUT pcName CHARACTER FUNCTION showmessage,LOGICAL,INPUT pcMessage CHARACTER FUNCTION setUserProperty,LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION setUIBMode,LOGICAL,INPUT pcMode CHARACTER FUNCTION setTranslatableProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setSupportedLinks,LOGICAL,INPUT pcLinkList CHARACTER FUNCTION setRunAttribute,LOGICAL,INPUT cRunAttribute CHARACTER FUNCTION setPhysicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setPhysicalObjectName,LOGICAL,INPUT cTemp CHARACTER FUNCTION setPassThroughLinks,LOGICAL,INPUT pcLinks CHARACTER FUNCTION setParentDataKey,LOGICAL,INPUT cParentDataKey CHARACTER FUNCTION setObjectVersion,LOGICAL,INPUT cObjectVersion CHARACTER FUNCTION setObjectParent,LOGICAL,INPUT phParent HANDLE FUNCTION setObjectName,LOGICAL,INPUT pcName CHARACTER FUNCTION setLogicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setLogicalObjectName,LOGICAL,INPUT c CHARACTER FUNCTION setInstanceProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setDynamicObject,LOGICAL,INPUT lTemp LOGICAL FUNCTION setDesignDataObject,LOGICAL,INPUT pcDataObject CHARACTER FUNCTION setDBAware,LOGICAL,INPUT lAware LOGICAL FUNCTION setDataTargetEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setDataTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setDataSourceNames,LOGICAL,INPUT pcSourceNames CHARACTER FUNCTION setDataSourceEvents,LOGICAL,INPUT pcEventsList CHARACTER FUNCTION setDataSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDataLinksEnabled,LOGICAL,INPUT lDataLinksEnabled LOGICAL FUNCTION setContainerSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setContainerSource,LOGICAL,INPUT phObject HANDLE FUNCTION setContainerHidden,LOGICAL,INPUT plHidden LOGICAL FUNCTION setChildDataKey,LOGICAL,INPUT cChildDataKey CHARACTER FUNCTION reviewMessages,CHARACTER, FUNCTION propertyType,CHARACTER,INPUT pcPropName CHARACTER FUNCTION messageNumber,CHARACTER,INPUT piMessage INTEGER FUNCTION mappedEntry,CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER FUNCTION linkProperty,CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER FUNCTION linkHandles,CHARACTER,INPUT pcLink CHARACTER FUNCTION instancePropertyList,CHARACTER,INPUT pcPropList CHARACTER FUNCTION getUserProperty,CHARACTER,INPUT pcPropName CHARACTER FUNCTION getUIBMode,CHARACTER, FUNCTION getTranslatableProperties,CHARACTER, FUNCTION getSupportedLinks,CHARACTER, FUNCTION getRunAttribute,CHARACTER, FUNCTION getQueryObject,LOGICAL, FUNCTION getPropertyDialog,CHARACTER, FUNCTION getPhysicalVersion,CHARACTER, FUNCTION getPhysicalObjectName,CHARACTER, FUNCTION getPassThroughLinks,CHARACTER, FUNCTION getParentDataKey,CHARACTER, FUNCTION getObjectVersionNumber,CHARACTER, FUNCTION getObjectVersion,CHARACTER, FUNCTION getObjectParent,HANDLE, FUNCTION getObjectPage,INTEGER, FUNCTION getObjectName,CHARACTER, FUNCTION getObjectInitialized,LOGICAL, FUNCTION getObjectHidden,LOGICAL, FUNCTION getLogicalVersion,CHARACTER, FUNCTION getLogicalObjectName,CHARACTER, FUNCTION getInstanceProperties,CHARACTER, FUNCTION getDynamicObject,LOGICAL, FUNCTION getDesignDataObject,CHARACTER, FUNCTION getDBAware,LOGICAL, FUNCTION getDataTargetEvents,CHARACTER, FUNCTION getDataTarget,CHARACTER, FUNCTION getDataSourceNames,CHARACTER, FUNCTION getDataSourceEvents,CHARACTER, FUNCTION getDataSource,HANDLE, FUNCTION getDataLinksEnabled,LOGICAL, FUNCTION getContainerType,CHARACTER, FUNCTION getContainerSourceEvents,CHARACTER, FUNCTION getContainerSource,HANDLE, FUNCTION getContainerHidden,LOGICAL, FUNCTION getContainerHandle,HANDLE, FUNCTION getChildDataKey,CHARACTER, FUNCTION fetchMessages,CHARACTER, FUNCTION assignLinkProperty,LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION anyMessage,LOGICAL, FUNCTION setServerOperatingMode,LOGICAL,INPUT pcServerOperatingMode CHARACTER FUNCTION setServerFileName,LOGICAL,INPUT pcFileName CHARACTER FUNCTION setASUsePrompt,LOGICAL,INPUT plFlag LOGICAL FUNCTION setASInitializeOnRun,LOGICAL,INPUT plInitialize LOGICAL FUNCTION setASInfo,LOGICAL,INPUT pcInfo CHARACTER FUNCTION setASHandle,LOGICAL,INPUT phASHandle HANDLE FUNCTION setASDivision,LOGICAL,INPUT pcDivision CHARACTER FUNCTION setAppService,LOGICAL,INPUT pcAppService CHARACTER FUNCTION runServerProcedure,HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE FUNCTION getServerOperatingMode,CHARACTER, FUNCTION getServerFileName,CHARACTER, FUNCTION getASUsePrompt,LOGICAL, FUNCTION getASInitializeOnRun,LOGICAL, FUNCTION getASInfo,CHARACTER, FUNCTION getASHasStarted,LOGICAL, FUNCTION getASHandle,HANDLE, FUNCTION getAsDivision,CHARACTER, FUNCTION getASBound,LOGICAL, FUNCTION getAppService,CHARACTER, FUNCTION whereClauseBuffer,CHARACTER,INPUT pcWhere CHARACTER FUNCTION setQueryWhere,LOGICAL,INPUT pcWhere CHARACTER FUNCTION setQueryString,LOGICAL,INPUT pcQueryString CHARACTER FUNCTION setQuerySort,LOGICAL,INPUT pcSort CHARACTER FUNCTION setQueryPosition,LOGICAL,INPUT pcPosition CHARACTER FUNCTION rowidWhereCols,CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER FUNCTION rowidWhere,CHARACTER,INPUT pcWhere CHARACTER FUNCTION resolveBuffer,CHARACTER,INPUT pcBuffer CHARACTER FUNCTION removeQuerySelection,LOGICAL,INPUT pcColumns CHARACTER,INPUT pcOperators CHARACTER FUNCTION removeForeignKey,LOGICAL, FUNCTION refreshRowident,CHARACTER,INPUT pcRowident CHARACTER FUNCTION newWhereClause,CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcExpression CHARACTER,INPUT pcWhere CHARACTER,INPUT pcAndOr CHARACTER FUNCTION newQueryWhere,CHARACTER,INPUT pcWhere CHARACTER FUNCTION newQueryValidate,CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcExpression CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER FUNCTION newQueryString,CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcAndOr CHARACTER FUNCTION insertExpression,CHARACTER,INPUT pcWhere CHARACTER,INPUT pcExpression CHARACTER,INPUT pcAndOr CHARACTER FUNCTION indexInformation,CHARACTER,INPUT pcQuery CHARACTER,INPUT plUseTableSep LOGICAL,INPUT pcIndexInfo CHARACTER FUNCTION getTargetProcedure,HANDLE, FUNCTION getQueryWhere,CHARACTER, FUNCTION getQueryString,CHARACTER, FUNCTION getQuerySort,CHARACTER, FUNCTION getQueryPosition,CHARACTER, FUNCTION getForeignValues,CHARACTER, FUNCTION getDataColumns,CHARACTER, FUNCTION excludeColumns,CHARACTER,INPUT iTable INTEGER FUNCTION dbColumnHandle,HANDLE,INPUT pcColumn CHARACTER FUNCTION dbColumnDataName,CHARACTER,INPUT pcDbColumn CHARACTER FUNCTION columnValMsg,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnTable,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnQuerySelection,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnDbColumn,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnDataType,CHARACTER,INPUT pcColumn CHARACTER FUNCTION bufferWhereClause,CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcWhere CHARACTER FUNCTION bufferCompareDBToRO,LOGICAL,INPUT phRowObjUpd HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER FUNCTION assignQuerySelection,LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER FUNCTION addQueryWhere,LOGICAL,INPUT pcWhere CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER FUNCTION getObjectType,character, FUNCTION updateRow,LOGICAL,INPUT pcKeyValues CHARACTER,INPUT pcValueList CHARACTER FUNCTION submitRow,LOGICAL,INPUT pcRowIdent CHARACTER,INPUT pcValueList CHARACTER FUNCTION rowValues,CHARACTER,INPUT pcColumns CHARACTER,INPUT pcFormat CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION rowAvailable,LOGICAL,INPUT pcDirection CHARACTER FUNCTION prepareQuery,LOGICAL,INPUT pcQuery CHARACTER FUNCTION openQuery,LOGICAL, FUNCTION openDataQuery,LOGICAL,INPUT pcPosition CHARACTER FUNCTION hasForeignKeyChanged,LOGICAL, FUNCTION getLastCommitErrorType,CHARACTER, FUNCTION firstRowIds,CHARACTER,INPUT pcQueryString CHARACTER FUNCTION findRowWhere,LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER FUNCTION findRow,LOGICAL,INPUT pcKeyValues CHARACTER FUNCTION fetchRowIdent,CHARACTER,INPUT pcRowIdent CHARACTER,INPUT pcViewColList CHARACTER FUNCTION fetchRow,CHARACTER,INPUT piRow INTEGER,INPUT pcViewColList CHARACTER FUNCTION deleteRow,LOGICAL,INPUT pcRowIdent CHARACTER FUNCTION createRow,LOGICAL,INPUT pcValueList CHARACTER FUNCTION copyRow,CHARACTER,INPUT pcViewColList CHARACTER FUNCTION colValues,CHARACTER,INPUT pcViewColList CHARACTER FUNCTION columnProps,CHARACTER,INPUT pcColList CHARACTER,INPUT pcPropList CHARACTER FUNCTION closeQuery,LOGICAL, FUNCTION canNavigate,LOGICAL, FUNCTION cancelRow,CHARACTER, FUNCTION addRow,CHARACTER,INPUT pcViewColList CHARACTER TEMP-TABLE RowObjUpd 0,RowNum:RowNum 0 NO,NroItm integer 0 0,codmat character 1 0,DesMat character 2 0,DesMar character 3 0,UndVta character 4 0,CanDes decimal 5 0,ImpLin decimal 6 0,RowNum integer 7 0,RowIdent character 8 0,RowMod character 9 0,RowIdentIdx character 10 0,RowUserProp character 11 0,ChangedFields character 12 0       �=              �)             � �=  �              (�              �B     +   � �  W   �� `  X   � �  Y   ��   [   ��   \   �� <  ]    �    ^    � 0  `   ? P� �!  iSO8859-1                                                                           H=    �                                      �                   ��                �=  @    t   �F   T�              ��  �   �=      �=                                                         PROGRESS                                    
    
                    �              �                                                                                                     
  4         �          X  �.     </     -  �.�d�/  A                     `          �      �   �  ~      ,  
    
                    �             �                                                                                          ~          
  \  �      �  
    
                  �  �             H                                                                                          �          
    �      �  
    
                  p  8             �                                                                                          �          
  �  �      0  
    
                    �             �                                                                                          �          
  `  �      �  
    
                  �  �             L                                                                                          �          
    �      �  
    
                  t  <  	           �                                                                                          �          
  �  �      4  
    
                     �  
           �                                                                                          �          
  d  �      �  
    
                  �  �             P                                                                                          �          
    	      �                         x  @             �                                                                                          	            �  	      8                        $  �             �                                                                                          	            h	  (	      �  
    
                  �  �	             T	                                                                                          (	          
  
  6	      �	  
    
                  |	  D
              
                                                                                          6	          
  �
  D	      <
  
    
                  (
  �
             �
                                                                                          D	          
  l  R	      �
                        �
  �             X                                                                                          R	              b	      �                        �  H                                                                                                       b	            �  m	      @                        ,  �             �                                                                                          m	                ~	      �                        �  p             \                                                                                          ~	            �         �         H  <6     \6    �      �6                      �0          �1      �              �       +  X  �<     �<  +  ��      =         +         �    �6           8      �                 P�                                               T�          �    L l�                �         
             
             
                                         
                                                                                                               
             
                                          L   \   l   |   �   �   �   �   �   �   �   �       ,  <  L  \      L   \   l   |   �   �   �   �   �   �   �   �      ,  <  L  \                                                                                                                                     	                                    
                                                                                                                                                                                                                                                                                                                                                                                                           !                 "                 #                  $                  %                  &                  '                  (                  )                  *                  +                  ,                  -                  .                  /                  0                  1                  2                  3                  4                  5                  6                  7                  8                  9                  :                  ;                  <                  =                  >                  ?                  @                  A                  B                                 �   �   �   �   �                          �    !  !  !  !                         !  $!  ,!  <!  4!                         @!  H!  L!  \!  T!                         `!  h!  p!  |!  x!          �!              �!  �!  �!  �!  �!                         �!  �!  �!  "  �!                         "  "  "  4"  ("                         8"  @"  H"  `"  T"                         d"  l"  |"  �"  �"                         �"  �"  �"  �"  �"                         �"  �"  �"  #  �"                         #  #   #  0#  (#                         4#  <#  D#  T#  L#                          X#  `#  h#  x#  p#                          |#  �#  �#  �#  �#                         �#  �#  �#  �#  �#                         �#  �#  $  $  $                          $  ($  8$  P$  D$                         T$  \$  h$  x$  p$          |$             �$  �$  �$  �$  �$                         �$  �$  �$  �$  �$                         �$  �$  �$  %   %          %              ,%  8%  H%  X%  P%                         \%  h%  p%  |%                              �%  �%  �%  �%  �%                         �%  �%  �%  �%  �%                         �%  �%  �%  �%  �%                          &  &  &  &                              &  (&  4&  <&                             @&  H&  X&  `&                             d&  l&  |&  �&                             �&  �&  �&  �&                             �&  �&  �&  �&                             �&  '  '  $'                              ('  <'  L'  `'                             d'  t'  �'  �'                             �'  �'  �'  �'                             �'  �'  �'  �'                               (  (  ,(  @(                       
      D(  T(  `(  p(                             t(  |(  �(  �(                             �(  �(  �(  �(                       
      �(  �(  )  ()                       
      ,)  D)  T)  l)                             p)  �)  �)  �)                             �)  �)  �)  �)                             �)   *  *   *                             $*  0*  @*  L*                             P*  h*  x*  �*                             �*  �*  �*  �*                       
      �*  +  +  0+                             4+  L+  `+  x+                             |+  �+  �+  �+                             �+  �+  �+  �+                             �+  ,  ,  $,                             (,  D,  T,  p,                             t,  �,  �,  �,                             �,  �,  �,   -                             -  -  (-  @-                              D-  `-  t-  �-                             �-  �-  �-  �-                             �-   .  .  0.                             4.  L.  T.  l.                             p.  �.  �.  �.                                                                         CodCia  999 Cia Cia 0   CodDoc  x(3)    Codigo  Codigo      NroDoc  X(12)   Numero  Numero      NroItm  >>9 No.Item No.Item 0   UndVta  x(8)    Unidad  Und     Unidad de movimiento    codmat  X(6)    Codigo Articulo Codigo Articulo     PreUni  >,>>>,>>9.999999    Precio Unitario Precio Unitario 0   PorDto  >>9.99  % Dscto.    % Dscto.    0   PorDto2 >>9.99  % Dscto.    % Dscto.    0   ImpDto  >,>>>,>>9.9999  Importe Descuento   Importe!Descuento   0   ImpLin  ->>,>>>,>>9.99  Importe Importe 0   CanDes  >,>>>,>>9.9999  Cantidad    Cantidad    0   Pesmat  ->>,>>9.9999    Peso    Peso    0   AftIgv  Si/No   I.G.V.  I.G.V.  Si  AftIsc  Si/No   I.S.C.  I.S.C.  No  PreBas  >,>>>,>>9.9999  Precio Base Precio Base 0   PreVta  >>,>>>,>>9.99   Precio Venta    Precio Venta    0   ImpIgv  >,>>>,>>9.9999  Importe Igv Importe Igv 0   ImpIsc  >,>>>,>>9.9999  Importe Isc Importe Isc 0   Factor  >>,>>9.9999 Factor  Factor  0   Factor  CanDev  >,>>>,>>9.9999  Cantidad    Cantidad    0   CodCli  x(11)   Cliente Cliente     AlmDes  x(3)    Almac�n Despacho    Almac�n!Despacho        Almac�n despacho    Por_Dsctos  ->,>>9.999999   % Dscto % Dscto 0   Flg_Factor  X(1)    Flg_Factor      CodDiv  XX-XXX  C.Div   C.Div   00000   FchDoc  99/99/9999  Fecha   Fecha   TODAY   ImpCto  ->>,>>>,>>9.99  ImpCto  ImpCto  0   puntos  ->>,>>9.99  puntos  0   mrguti  ->>,>>9.99  mrguti  0   ImpPro  ->>>,>>>,>>9.99 ImpPro  0   ImpDto2 ->>>,>>>,>>9.99 ImpDto2 0   PorDcto_Adelanto    ->>,>>9.99  PorDcto_Adelanto    0   ImpDcto_Adelanto    >>>,>>>,>>9.99  ImpDcto_Adelanto    0   Dcto_Otros_Mot  x(20)   Dcto_Otros_Mot      Dcto_Otros_Factor   ->>,>>9.999999  Dcto_Otros_Factor   0   Dcto_Otros_VV   ->>>,>>>,>>9.999999 Dcto_Otros_VV   0   Dcto_Otros_PV   ->>>,>>>,>>9.999999 Dcto_Otros_PV   0   cTipoAfectacion x(25)   cTipoAfectacion     cPreUniSinImpuesto  >>>>>>>>>>>9.9999999999 cPreUniSinImpuesto  0   FactorDescuento >>9.99999   FactorDescuento 0   TasaIGV >>9.99999   TasaIGV 0   ImporteUnitarioSinImpuesto  >>>>>>>>>>>9.9999999999 ImporteUnitarioSinImpuesto  0   ImporteReferencial  >>>>>>>>>>>9.9999999999 ImporteReferencial  0   ImporteBaseDescuento    >>>>>>>>>>>9.99 ImporteBaseDescuento    0   ImporteDescuento    >>>>>>>>>>>9.99 ImporteDescuento    0   ImporteTotalSinImpuesto >>>>>>>>>>>9.99 ImporteTotalSinImpuesto 0   MontoBaseIGV    >>>>>>>>>>>9.99 MontoBaseIGV    0   ImporteIGV  >>>>>>>>>>>9.99 ImporteIGV  0   ImporteTotalImpuestos   >>>>>>>>>>>9.99 ImporteTotalImpuestos   0   ImporteUnitarioConImpuesto  >>>>>>>>>>>9.99999999999    ImporteUnitarioConImpuesto  0   cImporteVentaExonerado  >>>,>>>,>>9.9999    cImporteVentaExonerado  0   cImporteVentaGratuito   >>>,>>>,>>9.9999    cImporteVentaGratuito   0   cSumaImpteTotalSinImpuesto  >>>,>>>,>>9.99  cSumaImpteTotalSinImpuesto  0   cMontoBaseIGV   >>>,>>>,>>9.99  cMontoBaseIGV   0   cSumaIGV    >>>,>>>,>>9.99  cSumaIGV    0   cOtrosTributosOpGratuito    >>>,>>>,>>9.99  cOtrosTributosOpGratuito    0   ImpuestoBolsaPlastico   >>>,>>>,>>9.99  ImpuestoBolsaPlastico   0   MontoTributoBolsaPlastico   >>>,>>>,>>9.99  MontoTributoBolsaPlastico   0   CantidadBolsaPlastico   ->,>>>,>>9  CantidadBolsaPlastico   0   MontoUnitarioBolsaPlastico  >>>,>>>,>>9.9999    MontoUnitarioBolsaPlastico  0   cImporteTotalConImpuesto    >>>,>>>,>>9.99  cImporteTotalConImpuesto    0   ImporteBaseDescuentoNoAfecto    >>>>>>>>>>>9.99 ImporteBaseDescuentoNoAfecto    0   FactorDescuentoNoAfecto >>9.99  FactorDescuentoNoAfecto 0   ImporteDescuentoNoAfecto    >>>>>>>>>>>9.99 ImporteDescuentoNoAfecto    0   �    A a q�  ���B������             �    �        �    � �00000     �      ��      �                                   �!        �!        �!        �!                �     i  i  i  i      i  i  i      i  i  i  i  i      i  i  i  i  i     	 	 	 	 	 	 	 	       "   )   0   7   >   E   L   [   b   i   w   ~   �   �   �   �   �   �   S   p   �   �   �   �   �   �   �   �   �   �   �       '  6  H  V  d  t  �  �  �  �  �  �  �      #  9  T  k  �  �  �  �  �  �  �    -  F  c  {                                                                                                                                     	                  
                                                                                       84  @4  D4  L4  H4                         P4  X4  `4  x4  l4                         |4  �4  �4  �4  �4          �4             �4  �4  �4  �4  �4                         �4  �4  �4  5   5          5             $5  ,5  <5  T5  H5                         X5  `5  p5  �5  �5                         �5  �5  �5  �5                             �5  �5  �5  �5                              �5  �5  �5  �5                             �5   6  6  6                             6  $6  ,6  86                                                                          NroItm  >>9 No  No  0   codmat  X(8)    Articulo    Articulo        DesMat  X(100)  Descripci�n Descripci�n     Descripci�n del material    DesMar  X(30)   Marca   Marca       UndVta  x(8)    Unidad  Unidad      Unidad de movimiento    CanDes  >,>>>,>>9.9999  Cantidad    Cantidad    0   ImpLin  ->>,>>>,>>9.99  Importe!con IGV Importe!con IGV 0   RowNum  ->,>>>,>>9  RowNum  0   RowIdent    x(8)    RowIdent        RowMod  x(8)    RowMod      RowIdentIdx x(8)    RowIdentIdx     RowUserProp x(8)    RowUserProp     �  ���������                   .         >         E                 �     i     i     i    	 	 	 	    0   >       '   7   i   b   .   5   >   E   Q                                                                                                                                      	                  
                                                                                                         h:  p:  t:  |:  x:                         �:  �:  �:  �:  �:                         �:  �:  �:  �:  �:          �:             �:  �:  ;  ;  ;                         ;   ;  (;  8;  0;          <;             T;  \;  l;  �;  x;                         �;  �;  �;  �;  �;                         �;  �;  �;  �;                             �;  �;  �;  <                              <  <  <   <                             $<  0<  8<  D<                             H<  T<  \<  h<                              l<  |<  �<  �<                                                                          NroItm  >>9 No  No  0   codmat  X(8)    Articulo    Articulo        DesMat  X(100)  Descripci�n Descripci�n     Descripci�n del material    DesMar  X(30)   Marca   Marca       UndVta  x(8)    Unidad  Unidad      Unidad de movimiento    CanDes  >,>>>,>>9.9999  Cantidad    Cantidad    0   ImpLin  ->>,>>>,>>9.99  Importe!con IGV Importe!con IGV 0   RowNum  ->,>>>,>>9  RowNum  0   RowIdent    x(8)    RowIdent        RowMod  x(8)    RowMod      RowIdentIdx x(8)    RowIdentIdx     RowUserProp x(8)    RowUserProp     ChangedFields   x(8)    ChangedFields       �  ���������                   .         >         E                 �     i     i     i    	 	 	 	    0   >       '   7   i   b   .   5   >   E   Q   ]     ��                            ����                            �!    p�                    �    undefined                                                               �       t�  �   l   ��  ��                    �����               ؄�                    O   ����    e�          O   ����    R�          O   ����    ��      t       �   �              4   ����     /                                     3   ����       $      H  ���                       8      
                       � ߱        �  �   "   D       �     E          ��    �   �  4      d       4   ����d                 D                      ��                  �   �                   � �                       �   �  �  	  �   x                                        3   ����|       O   �   ��  ��  �   batchServices                               4        ��                  b  e  L              4[�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             d               ��                  �           ��                            ����                            clientSendRows                              �  p      ��                  g  m  �              42�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             �               ��                �               ��   <                            ��   d             0               ��                  X           ��                            ����                            commitTransaction                               X  @      ��                  o  p  p              �a�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            copyColumns                             X  @      ��                  r  u  p              4d�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             �               �� 
                 �  
         ��                            ����                            dataAvailable                               �  �      ��                  w  y  �              �n�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            describeSchema                              �	  �	      ��                  {  ~  �	              t:�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   <
             
               �� 
          �       0
  
         ��                            ����                            destroyServerObject                             0        ��                  �  �  H              �t�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            endClientDataRequest                                <  $      ��                  �  �  T              lK�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchBatch                              <  $      ��                  �  �  T              L�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  l           ��                            ����                            fetchFirst                              d  L      ��                  �  �  |              �x�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchLast                               d  L      ��                  �  �  |              @?�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchNext                               d  L      ��                  �  �  |              �?�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchPrev                               d  L      ��                  �  �  |              �@�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            genContextList                              h  P      ��                  �  �  �              �                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            home                                �  t      ��                  �  �  �              �S�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �  |      ��                  �  �  �              �T�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              �  �      ��                  �  �  �              �{�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            isUpdatePending                             �  �      ��                  �  �  �              ||�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            printToCrystal                              �  �      ��                  �  �  �              �                    O   ����    e�          O   ����    R�          O   ����    ��            ��   4                             ��   \             (               ��                  P           ��                            ����                            refreshRow                              H  0      ��                  �  �  `              <��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            remoteSendRows                              L  4      ��                  �  �  d               ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             |               ��   �             �               ��                 �               ��   (             �               ��   P                            ��   x             D               �� 
  �      �       l  
             ��                  �           ��                            ����                            restartServerObject                             �  |      ��                  �  �  �              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            retrieveFilter                              �  �      ��                  �  �  �              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            rowObjectState                              �  �      ��                  �  �  �              HĆ                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            saveContextAndDestroy                               �  �      ��                  �  �  �              ؋�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                               ��                            ����                            serverSendRows                              �   �       ��                  �  �  !              ���                    O   ����    e�          O   ����    R�          O   ����    ��            ��   `!             ,!               ��   �!             T!               ��   �!             |!               ��   �!             �!               ��    "             �!               �� 
          �       �!  
         ��                            ����                            serverFetchRowObjUpdTable                               �"  �"      ��                  �  �  #              `��                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
          �       ,#  
         ��                            ����                            setPropertyList                             ($  $      ��                  �  �  @$              ���                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  X$           ��                            ����                            serverSendRows                              T%  <%      ��                  �  �  l%              ,��                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �%             �%               ��   �%             �%               ��   &             �%               ��   0&             �%               ��   X&             $&               �� 
          �       L&  
         ��                            ����                            startServerObject                               L'  4'      ��                  �  �  d'              ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            submitCommit                                P(  8(      ��                  �  �  h(              䇆                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �(             �(               ��                  �(           ��                            ����                            submitForeignKey                                �)  �)      ��                  �     �)              �Ɔ                    O   ����    e�          O   ����    R�          O   ����    ��            ��   *             �)               ��   4*              *               ��                  (*           ��                            ����                            submitValidation                                (+  +      ��                      @+              l҆                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �+             X+               ��                  �+           ��                            ����                            synchronizeProperties                               �,  l,      ��                    
  �,              pa�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �,             �,               ��                  �,           ��                            ����                            transferToExcel                             �-  �-      ��                      �-              D�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   <.             .               ��   d.             0.               ��   �.             X.               ��                  �.           ��                            ����                            undoTransaction                             |/  d/      ��                      �/              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            updateAddQueryWhere                             �0  l0      ��                    !  �0              p�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �0             �0               ��                  �0           ��                            ����                            updateQueryPosition                             �1  �1      ��                  #  $  �1              �V�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            updateState                             �2  �2      ��                  &  (  �2              �                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  3           ��                            ����                            addRow          t3      �3     �      CHARACTER,INPUT pcViewColList CHARACTER cancelRow   |3      �3      �3   	 �      CHARACTER,  canNavigate �3      �3      (4    �      LOGICAL,    closeQuery  4      44      `4   
       LOGICAL,    columnProps @4      l4      �4          CHARACTER,INPUT pcColList CHARACTER,INPUT pcPropList CHARACTER  colValues   x4      �4      5   	       CHARACTER,INPUT pcViewColList CHARACTER copyRow �4      ,5      T5    %      CHARACTER,INPUT pcViewColList CHARACTER createRow   45      |5      �5   	 -      LOGICAL,INPUT pcValueList CHARACTER deleteRow   �5      �5      �5   	 7      LOGICAL,INPUT pcRowIdent CHARACTER  fetchRow    �5      6      H6  	  A      CHARACTER,INPUT piRow INTEGER,INPUT pcViewColList CHARACTER fetchRowIdent   (6      �6      �6  
  J      CHARACTER,INPUT pcRowIdent CHARACTER,INPUT pcViewColList CHARACTER  findRow �6      �6       7    X      LOGICAL,INPUT pcKeyValues CHARACTER findRowWhere     7      D7      t7    `      LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER  firstRowIds T7      �7      �7    m      CHARACTER,INPUT pcQueryString CHARACTER getLastCommitErrorType  �7       8      X8    y      CHARACTER,  hasForeignKeyChanged    88      d8      �8    �      LOGICAL,    openDataQuery   |8      �8      �8    �      LOGICAL,INPUT pcPosition CHARACTER  openQuery   �8      �8      (9   	 �      LOGICAL,    prepareQuery    9      49      d9    �      LOGICAL,INPUT pcQuery CHARACTER rowAvailable    D9      �9      �9    �      LOGICAL,INPUT pcDirection CHARACTER rowValues   �9      �9      :   	 �      CHARACTER,INPUT pcColumns CHARACTER,INPUT pcFormat CHARACTER,INPUT pcDelimiter CHARACTER    submitRow   �9      `:      �:   	 �      LOGICAL,INPUT pcRowIdent CHARACTER,INPUT pcValueList CHARACTER  updateRow   l:      �:      �:   	 �      LOGICAL,INPUT pcKeyValues CHARACTER,INPUT pcValueList CHARACTER getObjectType   �:      8;      h;    �      CHARACTER,  assignDBRow                             <  �;      ��                      <              @�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 4<  
         ��                            ����                            bufferCopyDBToRO                                4=  =      ��                      L=              �3�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �=             d=  
             �� 
  �=             �=  
             ��   �=             �=               ��                  �=           ��                            ����                            compareDBRow                                �>  �>      ��                      �>              ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            confirmContinue                             �?  �?      ��                      �?              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  @           ��                            ����                            dataAvailable                               A  �@      ��                     "   A              ���                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  8A           ��                            ����                            fetchDBRowForUpdate                             8B   B      ��                  $  %  PB              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchFirst                              8C   C      ��                  '  (  PC              D�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchLast                               8D   D      ��                  *  +  PD              X·                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchNext                               8E   E      ��                  -  .  PE              Pχ                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchPrev                               8F   F      ��                  0  1  PF              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            filterContainerHandler                              DG  ,G      ��                  3  5  \G              ��                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 tG  
         ��                            ����                            initializeObject                                tH  \H      ��                  7  8  �H              `҇                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            refetchDBRow                                xI  `I      ��                  :  <  �I              hӇ                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �I  
         ��                            ����                            releaseDBRow                                �J  �J      ��                  >  ?  �J              ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            startFilter                             �K  �K      ��                  A  B  �K              �                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            transferDBRow                               �L  �L      ��                  D  G  �L              ,Ç                    O   ����    e�          O   ����    R�          O   ����    ��            ��   M             �L               ��                   M           ��                            ����                            addQueryWhere   H;      hM      �M          LOGICAL,INPUT pcWhere CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER    assignQuerySelection    xM      �M      $N    $      LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER  bufferCompareDBToRO N      |N      �N    9      LOGICAL,INPUT phRowObjUpd HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER bufferWhereClause   �N      O      PO    M      CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcWhere CHARACTER  columnDataType  0O      �O      �O    _      CHARACTER,INPUT pcColumn CHARACTER  columnDbColumn  �O      �O      P    n      CHARACTER,INPUT pcColumn CHARACTER  columnQuerySelection    �O      4P      lP    }      CHARACTER,INPUT pcColumn CHARACTER  columnTable LP      �P      �P    �      CHARACTER,INPUT pcColumn CHARACTER  columnValMsg    �P      �P      Q     �      CHARACTER,INPUT pcColumn CHARACTER  dbColumnDataName    �P      4Q      hQ  !  �      CHARACTER,INPUT pcDbColumn CHARACTER    dbColumnHandle  HQ      �Q      �Q  "  �      HANDLE,INPUT pcColumn CHARACTER excludeColumns  �Q      �Q      R  #  �      CHARACTER,INPUT iTable INTEGER  getDataColumns  �Q      0R      `R  $  �      CHARACTER,  getForeignValues    @R      lR      �R  %  �      CHARACTER,  getQueryPosition    �R      �R      �R  &  �      CHARACTER,  getQuerySort    �R      �R      S  '        CHARACTER,  getQueryString  �R      (S      XS  (        CHARACTER,  getQueryWhere   8S      dS      �S  )  '      CHARACTER,  getTargetProcedure  tS      �S      �S  *  5      HANDLE, indexInformation    �S      �S      T  +  H      CHARACTER,INPUT pcQuery CHARACTER,INPUT plUseTableSep LOGICAL,INPUT pcIndexInfo CHARACTER   insertExpression    �S      lT      �T  ,  Y      CHARACTER,INPUT pcWhere CHARACTER,INPUT pcExpression CHARACTER,INPUT pcAndOr CHARACTER  newQueryString  �T      �T      (U  -  j      CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcAndOr CHARACTER  newQueryValidate    U      �U      �U  .  y      CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcExpression CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER   newQueryWhere   �U      dV      �V  /  �      CHARACTER,INPUT pcWhere CHARACTER   newWhereClause  tV      �V      �V  0  �      CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcExpression CHARACTER,INPUT pcWhere CHARACTER,INPUT pcAndOr CHARACTER refreshRowident �V      XW      �W  1  �      CHARACTER,INPUT pcRowident CHARACTER    removeForeignKey    hW      �W      �W  2  �      LOGICAL,    removeQuerySelection    �W      �W      (X  3  �      LOGICAL,INPUT pcColumns CHARACTER,INPUT pcOperators CHARACTER   resolveBuffer   X      hX      �X  4  �      CHARACTER,INPUT pcBuffer CHARACTER  rowidWhere  xX      �X      �X  5 
 �      CHARACTER,INPUT pcWhere CHARACTER   rowidWhereCols  �X      Y      <Y  6  �      CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER    setQueryPosition    Y      �Y      �Y  7        LOGICAL,INPUT pcPosition CHARACTER  setQuerySort    �Y      �Y       Z  8        LOGICAL,INPUT pcSort CHARACTER  setQueryString   Z      @Z      pZ  9  #      LOGICAL,INPUT pcQueryString CHARACTER   setQueryWhere   PZ      �Z      �Z  :  2      LOGICAL,INPUT pcWhere CHARACTER whereClauseBuffer   �Z      �Z      [  ;  @      CHARACTER,INPUT pcWhere CHARACTER   bindServer                              �[  �[      ��                  �  �  �[              ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �\  �\      ��                  �  �  �\              �_�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyServerObject                             �]  �]      ��                  �  �  �]              h`�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disconnectObject                                �^  �^      ��                  �  �  �^              �g�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              �_  �_      ��                  �  �  `              th�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            restartServerObject                             �`  �`      ��                  �  �  a              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            runServerObject                             �a  �a      ��                  �  �  b              <��                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 ,b  
         ��                            ����                            startServerObject                               ,c  c      ��                  �  �  Dc              菄                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            unbindServer                                0d  d      ��                  �  �  Hd               ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  `d           ��                            ����                            getAppService   �Z      �d      �d  <  R      CHARACTER,  getASBound  �d      e      0e  = 
 `      LOGICAL,    getAsDivision   e      <e      le  >  k      CHARACTER,  getASHandle Le      xe      �e  ?  y      HANDLE, getASHasStarted �e      �e      �e  @  �      LOGICAL,    getASInfo   �e      �e      f  A 	 �      CHARACTER,  getASInitializeOnRun    �e       f      Xf  B  �      LOGICAL,    getASUsePrompt  8f      df      �f  C  �      LOGICAL,    getServerFileName   tf      �f      �f  D  �      CHARACTER,  getServerOperatingMode  �f      �f      g  E  �      CHARACTER,  runServerProcedure  �f      $g      Xg  F  �      HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE   setAppService   8g      �g      �g  G  �      LOGICAL,INPUT pcAppService CHARACTER    setASDivision   �g      �g      $h  H        LOGICAL,INPUT pcDivision CHARACTER  setASHandle h      Hh      th  I        LOGICAL,INPUT phASHandle HANDLE setASInfo   Th      �h      �h  J 	 '      LOGICAL,INPUT pcInfo CHARACTER  setASInitializeOnRun    �h      �h      i  K  1      LOGICAL,INPUT plInitialize LOGICAL  setASUsePrompt  �h      <i      li  L  F      LOGICAL,INPUT plFlag LOGICAL    setServerFileName   Li      �i      �i  M  U      LOGICAL,INPUT pcFileName CHARACTER  setServerOperatingMode  �i      �i      j  N  g      LOGICAL,INPUT pcServerOperatingMode CHARACTER   addLink                             �j  �j      ��                  �  �  �j              �O�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  <k             k  
             ��   dk             0k               �� 
                 Xk  
         ��                            ����                            addMessage                              Pl  8l      ��                  �  �  hl              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �l             �l               ��   �l             �l               ��                  �l           ��                            ����                            adjustTabOrder                              �m  �m      ��                  �  �  �m              T�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  0n             �m  
             �� 
  Xn             $n  
             ��                  Ln           ��                            ����                            applyEntry                              Do  ,o      ��                  �  �  \o              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  to           ��                            ����                            changeCursor                                pp  Xp      ��                  �  �  �p              �Y�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �p           ��                            ����                            createControls                              �q  �q      ��                  �  �  �q              �=�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �r  �r      ��                  �  �  �r              �>�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            displayLinks                                �s  �s      ��                  �  �  �s              �E�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            editInstanceProperties                              �t  �t      ��                  �  �  �t              �F�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            exitObject                              �u  �u      ��                  �  �  �u              $�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hideObject                              �v  �v      ��                  �  �  �v              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �w  �w      ��                  �  �  �w              HA�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            modifyListProperty                              �x  �x      ��                  �  �  �x              B�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  $y             �x  
             ��   Ly             y               ��   ty             @y               ��                  hy           ��                            ����                            modifyUserLinks                             dz  Lz      ��                  �  �  |z              k�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �z             �z               ��   �z             �z               �� 
                 �z  
         ��                            ����                            removeAllLinks                              �{  �{      ��                  �  �  �{              �r�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            removeLink                              �|  �|      ��                  �    �|              s�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  D}             }  
             ��   l}             8}               �� 
                 `}  
         ��                            ����                            repositionObject                                `~  H~      ��                      x~              �.                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �~             �~               ��                  �~           ��                            ����                            returnFocus                             �  �      ��                  	    �              �.                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �  
         ��                            ����                            showMessageProcedure                                �  ̀      ��                      ��              �.                    O   ����    e�          O   ����    R�          O   ����    ��            ��   H�             �               ��                  <�           ��                            ����                            toggleData                              4�  �      ��                      L�              آ-                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  d�           ��                            ����                            viewObject                              \�  D�      ��                      t�              ��-                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            anyMessage  �i      ̃      ��  O 
 �	      LOGICAL,    assignLinkProperty  ؃      �      8�  P  �	      LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER   fetchMessages   �      ��      ��  Q  �	      CHARACTER,  getChildDataKey ��      ̄      ��  R  �	      CHARACTER,  getContainerHandle  ܄      �      <�  S  
      HANDLE, getContainerHidden  �      D�      x�  T  
      LOGICAL,    getContainerSource  X�      ��      ��  U  .
      HANDLE, getContainerSourceEvents    ��      ��      ��  V  A
      CHARACTER,  getContainerType    ܅      �      <�  W  Z
      CHARACTER,  getDataLinksEnabled �      H�      |�  X  k
      LOGICAL,    getDataSource   \�      ��      ��  Y  
      HANDLE, getDataSourceEvents ��      ��      �  Z  �
      CHARACTER,  getDataSourceNames  Ԇ       �      4�  [  �
      CHARACTER,  getDataTarget   �      @�      p�  \  �
      CHARACTER,  getDataTargetEvents P�      |�      ��  ]  �
      CHARACTER,  getDBAware  ��      ��      �  ^ 
 �
      LOGICAL,    getDesignDataObject ȇ      �      (�  _  �
      CHARACTER,  getDynamicObject    �      4�      h�  `  �
      LOGICAL,    getInstanceProperties   H�      t�      ��  a        CHARACTER,  getLogicalObjectName    ��      ��      ��  b        CHARACTER,  getLogicalVersion   Ј      ��      0�  c  1      CHARACTER,  getObjectHidden �      <�      l�  d  C      LOGICAL,    getObjectInitialized    L�      x�      ��  e  S      LOGICAL,    getObjectName   ��      ��      �  f  h      CHARACTER,  getObjectPage   ̉      ��      (�  g  v      INTEGER,    getObjectParent �      4�      d�  h  �      HANDLE, getObjectVersion    D�      l�      ��  i  �      CHARACTER,  getObjectVersionNumber  ��      ��      �  j  �      CHARACTER,  getParentDataKey    Ċ      ��      $�  k  �      CHARACTER,  getPassThroughLinks �      0�      d�  l  �      CHARACTER,  getPhysicalObjectName   D�      p�      ��  m  �      CHARACTER,  getPhysicalVersion  ��      ��      �  n  �      CHARACTER,  getPropertyDialog   ȋ      �      (�  o  
      CHARACTER,  getQueryObject  �      4�      d�  p        LOGICAL,    getRunAttribute D�      p�      ��  q  +      CHARACTER,  getSupportedLinks   ��      ��      ��  r  ;      CHARACTER,  getTranslatableProperties   ��      �      (�  s  M      CHARACTER,  getUIBMode  �      4�      `�  t 
 g      CHARACTER,  getUserProperty @�      l�      ��  u  r      CHARACTER,INPUT pcPropName CHARACTER    instancePropertyList    |�      č      ��  v  �      CHARACTER,INPUT pcPropList CHARACTER    linkHandles ܍      $�      P�  w  �      CHARACTER,INPUT pcLink CHARACTER    linkProperty    0�      t�      ��  x  �      CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER mappedEntry ��      ��      �  y  �      CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER  messageNumber   �      x�      ��  z  �      CHARACTER,INPUT piMessage INTEGER   propertyType    ��      ̏      ��  {  �      CHARACTER,INPUT pcPropName CHARACTER    reviewMessages  ܏      $�      T�  |  �      CHARACTER,  setChildDataKey 4�      `�      ��  }  �      LOGICAL,INPUT cChildDataKey CHARACTER   setContainerHidden  p�      ��      �  ~  �      LOGICAL,INPUT plHidden LOGICAL  setContainerSource  ̐      �      @�    	      LOGICAL,INPUT phObject HANDLE   setContainerSourceEvents     �      `�      ��  �        LOGICAL,INPUT pcEvents CHARACTER    setDataLinksEnabled |�      ��      ��  �  5      LOGICAL,INPUT lDataLinksEnabled LOGICAL setDataSource   ԑ      �      L�  �  I      LOGICAL,INPUT phObject HANDLE   setDataSourceEvents ,�      l�      ��  �  W      LOGICAL,INPUT pcEventsList CHARACTER    setDataSourceNames  ��      Ȓ      ��  �  k      LOGICAL,INPUT pcSourceNames CHARACTER   setDataTarget   ܒ      $�      T�  �  ~      LOGICAL,INPUT pcTarget CHARACTER    setDataTargetEvents 4�      x�      ��  �  �      LOGICAL,INPUT pcEvents CHARACTER    setDBAware  ��      Г      ��  � 
 �      LOGICAL,INPUT lAware LOGICAL    setDesignDataObject ܓ      �      P�  �  �      LOGICAL,INPUT pcDataObject CHARACTER    setDynamicObject    0�      x�      ��  �  �      LOGICAL,INPUT lTemp LOGICAL setInstanceProperties   ��      Ȕ       �  �  �      LOGICAL,INPUT pcPropList CHARACTER  setLogicalObjectName    ��      $�      \�  �  �      LOGICAL,INPUT c CHARACTER   setLogicalVersion   <�      x�      ��  �  �      LOGICAL,INPUT cVersion CHARACTER    setObjectName   ��      Е       �  �        LOGICAL,INPUT pcName CHARACTER  setObjectParent ��       �      P�  �        LOGICAL,INPUT phParent HANDLE   setObjectVersion    0�      p�      ��  �  +      LOGICAL,INPUT cObjectVersion CHARACTER  setParentDataKey    ��      ̖       �  �  <      LOGICAL,INPUT cParentDataKey CHARACTER  setPassThroughLinks ��      (�      \�  �  M      LOGICAL,INPUT pcLinks CHARACTER setPhysicalObjectName   <�      |�      ��  �  a      LOGICAL,INPUT cTemp CHARACTER   setPhysicalVersion  ��      ԗ      �  �  w      LOGICAL,INPUT cVersion CHARACTER    setRunAttribute �      ,�      \�  �  �      LOGICAL,INPUT cRunAttribute CHARACTER   setSupportedLinks   <�      ��      ��  �  �      LOGICAL,INPUT pcLinkList CHARACTER  setTranslatableProperties   ��      ܘ      �  �  �      LOGICAL,INPUT pcPropList CHARACTER  setUIBMode  ��      <�      h�  � 
 �      LOGICAL,INPUT pcMode CHARACTER  setUserProperty H�      ��      ��  �  �      LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER  showmessage ��      ��      $�  �  �      LOGICAL,INPUT pcMessage CHARACTER   Signature   �      H�      t�  � 	 �      CHARACTER,INPUT pcName CHARACTER    l�    -  ��  0�      �       4   �����                 @�                      ��                  .  [                  �-                       .  Ě        /  \�  ؛      �       4   �����                 �                      ��                  0  Z                  h�-                       0  l�  �    G  �  ��      �       4   �����                 ��                      ��                  S  U                  �-                       S  �         T                                  ,     
                    � ߱        �  $  W  ��  ���                           $  Y  @�  ���                       x                         � ߱        x�    _  ��  �      �      4   �����                �                      ��                  `  $	                  ��-                       `  ��  H�  o   c      ,                                 ��  $   d  t�  ���                       �  @         �              � ߱        ��  �   e        Ȟ  �   f  �      ܞ  �   h        �  �   j  x      �  �   l  �      �  �   n  `      ,�  �   o  �      @�  �   p        T�  �   s  �      h�  �   u         |�  �   v  |      ��  �   x  �      ��  �   y  t      ��  �   z  �      ̟  �   {  ,      ��  �   |  �      ��  �   �  �      �  �   �  P	      �  �   �  �	      0�  �   �   
      D�  �   �  t
      X�  �   �  �
      l�  �   �  l      ��  �   �  �      ��  �   �  \      ��  �   �  �      ��  �   �  D      Р  �   �  �      �  �   �  �      ��  �   �  0      �  �   �  �       �  �   �  �      4�  �   �        H�  �   �  X      \�  �   �  �      p�  �   �        ��  �   �  L      ��  �   �  �      ��  �   �  �      ��  �   �         ԡ  �   �  <      �  �   �  x      ��  �   �  �      �  �   �  �          �   �  ,                      <�          ��  ��      ��                  K	  y	  ��              ��-                    O   ����    e�          O   ����    R�          O   ����    ��      �     
                                     (                         � ߱        h�  $ _	  آ  ���                           O   w	  ��  ��  h               ԣ          ģ  ̣    ��                                             ��                            ����                                8;      $�      ��     V     ܣ                       أ  �                     8�    �	  ��  �      t      4   ����t                 �                      ��                  �	   
                  ,�.                       �	  ��  4�  �   �	  �      H�  �   �	  H      \�  �   �	  �      p�  �   �	  @      ��  �   �	  �      ��  �   �	  8      ��  �   �	  �      ��  �   �	  (      ԥ  �   �	  �      �  �   �	         ��  �   �	  �      �  �   �	        $�  �   �	  �          �   �	        ��    P
  T�  Ц      x      4   ����x                �                      ��                  Q
  �
                  ��.                       Q
  d�  ��  �   S
  �      �  �   T
  T      �  �   U
  �      0�  �   V
  D      D�  �   W
  �      X�  �   X
  �      l�  �   Z
  p      ��  �   [
  �      ��  �   \
  X      ��  �   ]
  �      ��  �   ^
  �      Ч  �   _
  D       �  �   `
  �       ��  �   a
  �       �  �   b
  x!       �  �   c
  �!      4�  �   d
  h"      H�  �   e
  �"      \�  �   f
  `#      p�  �   g
  �#      ��  �   h
  X$      ��  �   i
  �$      ��  �   j
  �$      ��  �   k
  L%      Ԩ  �   l
  �%      �  �   m
  <&      ��  �   n
  �&      �  �   o
  4'      $�  �   p
  �'      8�  �   q
  ,(      L�  �   r
  h(      `�  �   t
  �(      t�  �   u
  X)      ��  �   v
  �)      ��  �   w
  *      ��  �   x
  �*      ĩ  �   y
  �*      ة  �   z
  l+      �  �   {
  �+       �  �   |
  \,      �  �   }
  �,      (�  �   ~
  L-      <�  �   
  �-      P�  �   �
  <.      d�  �   �
  �.      x�  �   �
  4/      ��  �   �
  �/          �   �
  $0      d�    �
  ��  8�      T0      4   ����T0                H�                      ��                  �
  �                  ��.                       �
  ̪  \�  �     �0      p�  �     (1      ��  �     �1      ��  �     2      ��  �     �2      ��  �     3      ԫ  �     |3      �  �   	  �3      ��  �   
  t4      �  �     �4      $�  �     l5      8�  �     �5      L�  �     d6      `�  �     �6      t�  �     L7      ��  �     �7      ��  �     <8      ��  �     �8      Ĭ  �     ,9      ج  �     �9      �  �     :       �  �     X:      �  �     �:      (�  �     H;      <�  �     �;      P�  �     8<          �     �<      x�    �  ��  ��      =      4   ����=  	              �                      ��             	     �  <                  .                       �  ��   �  �   �  |=      4�  �   �  �=      H�  �   �  t>      \�  �   �  �>      p�  �   �  l?      ��  �   �  �?      ��  �   �  \@      ��  �   �  �@      ��  �   �  TA      Ԯ  �   �  �A      �  �   �  DB      ��  �   �  �B      �  �   �  <C      $�  �   �  �C      8�  �   �  ,D      L�  �   �  �D      `�  �   �  $E      t�  �   �  �E      ��  �   �  F      ��  �   �  �F      ��  �   �  G      į  �   �  �G      د  �   �  �G      �  �   �  8H       �  �   �  �H      �  �   �  0I      (�  �   �  �I      <�  �   �   J          �   �  �J      getRowObjUpdStatic  deleteRecordStatic  �    �  ��  ��      K      4   ����K      /   �  а     �                          3   ����K             �                      3   ����<K  Զ    �  ,�  ��  �  XK      4   ����XK  
              ��                      ��             
     �  ^                  �(.                       �  <�  ̱  �      �K      $�  $    ��  ���                       �K     
                    � ߱        8�  �     L      ��  $     d�  ���                       ,L  @         L              � ߱        L�  $    ��  ���                       �L       	       	           � ߱        �M     
                N                     \O  @        
 O              � ߱        ܳ  V     �  ���                        hO       	       	       �O       
       
       �O       	       	           � ߱        l�  $  -  x�  ���                       �P     
                Q                     dR  @        
 $R              � ߱            V   ?  �  ���                                      ̵                      ��                  `  �                  �@.                       `  ��  pR     
                �R                     <T  @        
 �S          �T  @        
 dT          U  @        
 �T          dU  @        
 $U              � ߱            V   u  �  ���                        adm-clone-props �  ��              �     W     `                          \  �                     start-super-proc    �  d�  �           �     X                                                       l�      �   �      �X      4   �����X      /     ,�     <�                          3   ���� Y            \�                      3   ���� Y  ķ  $  0  ��  ���                       @Y                         � ߱        ��    @  �  \�  ��  \Y      4   ����\Y                и                      ��                  A  E                  �0.                       A  �  pY                     �Y                     �Y                         � ߱            $  B  l�  ���                             F  �  T�      �Y      4   �����Y  �Y                         � ߱            $  G  (�  ���                       �Y                         � ߱        ع  $  K  ��  ���                       Ժ    N  ��  �  \�  �Y      4   �����Y      $  O  0�  ���                       Z                         � ߱            �   l  ,Z      lZ     
                �Z                     8\  @        
 �[              � ߱         �  V   �  p�  ���                        �  �   �  D\      �    2  0�  @�      �\      4   �����\      /   3  l�     |�                          3   �����\            ��                      3   �����\  �\     
                P]                     �^  @        
 `^              � ߱        ��  V   ?  ��  ���                        �^     
                h_                     �`  @        
 x`              � ߱        ̼  V   c  <�  ���                        L�    �  �  d�      �`      4   �����`                t�                      ��                  �  �                  ��.                       �  ��  �  /   �  ��     ��                          3   �����`            н                      3   �����`      /   �  �     �                          3   ����a            <�                      3   ����8a  ��  /  P  x�         la                      3   ����Ta  initProps   x�  ��              H     Y     @                          <  �  	                                   ̿          t�  \�      ��                     ��              ��.                    O   ����    e�          O   ����    R�          O   ����    ��      �                      ��          ��  p     (t  �        t�  d�     4t                                        ��                    -                  $�.                         ��   �  ��     Ht                                        ��                  .  J                  ��.                       .  ��  ��  |�     \t                                        ��                  K  g                  <�.                       K  �  �  �     pt                                        ��                  h  �                  ��.                       h  ��  ��  ��     �t                                        ��                  �  �                  ��.                       �  (�  0�   �     �t                                        ��                  �  �                  ��.                       �  ��  ��  ��     �t                                        ��                  �  �                  d�.                       �  @�  H�  8�     �t                                        ��                  �  �                  t�/                       �  ��  ��  ��     �t  	                                      ��             	     �                    D�/                       �  X�  `�  P�     �t  
                                      ��             
       2                  �/                         ��  ��  ��     �t                                        ��                  3  O                  �/                       3  p�  x�  h�     u                                        ��                  P  l                  ��/                       P  ��  �  ��     $u                                        ��                  m  �                   $/                       m  ��  ��  ��     8u                                        ��                  �  �                  �$/                       �  �  �  �     Lu                                        ��                  �  �                  �%/                       �  ��  ��  ��     `u                                        ��                  �  �                  p&/                       �  ,�  4�  $�     tu                                        ��                  �  �                  �;/                       �  ��      ��     �u                                        ��                  �                    t</                       �  D�      O     ��  ��  �u               D�          ,�  8�   , �                                                       �     ��                            ����                            ��  d�  ��  �      ��     Z     L�                      � H�  �                     ��    3  �  ��      �u      4   �����u                ��                      ��                  4  H                  �=/                       4  �  ��  /   5  ��     ��                          3   �����u            ��                      3   �����u  h�  /   6  (�     8�                          3   �����u            X�                      3   ����v  ��  /   ;  ��     ��                          3   ����,v            ��                      3   ����Lv      /   A   �     �                          3   ����lv            0�                      3   �����v  �v     
                (w                     xx  @        
 8x              � ߱        ��  V   �  @�  ���                        ��  $  �  ��  ���                       �x                         � ߱        �x     
                4y                     �z  @        
 Dz              � ߱        ��  V   �  (�  ���                        t�  $  �  ��  ���                       �z     
                    � ߱        �z     
                 {                     p|  @        
 0|              � ߱        ��  V   �  �  ���                        \�  $  �  ��  ���                       ||     
                    � ߱        �|     
                }                     \~  @        
 ~              � ߱        ��  V     ��  ���                        D�  $     ��  ���                       t~                         � ߱        �~     
                                     h�  @        
 (�              � ߱        p�  V   *  ��  ���                        ��  �   D  ��      @�  $  E  ��  ���                       ��     
                    � ߱        ��     
                0�                     ��  @        
 @�              � ߱        l�  V   O  ��  ���                        ��  $  i  ��  ���                       ��     
                    � ߱        ��  �   �  ��      0�  $  �  �  ���                       ��     
                    � ߱        D�  �   �  �      ��  $  �  p�  ���                       4�                         � ߱              �  ��  ��      P�      4   ����P�      /   �  ��     �                          3   ����p�  4�     
   $�                      3   ������  d�        T�                      3   ������  ��        ��                      3   ������            ��                      3   ����ȃ  pushRowObjUpdTable  ��  ��  �                   [      �                               �                      pushTableAndValidate    ��  4�  �           |     \     �                          �  !                     remoteCommit    L�  ��  �           p     ]     �                          �  ^!                     serverCommit    ��  �  �           l     ^     �                          �  k!                                     4�          �  ��      ��                  �    �              <x/                    O   ����    e�          O   ����    R�          O   ����    ��          O     ��  ��  ��    ��                            ����                            $�  P�      ��              _      L�                      
�     x!                     disable_UI  ��  ��                      `      �                               �!  
                    �  �    ����  �       ��       ��  8   ����   ��  8   ����       8   ����       8   ����       ��  ��      viewObject  ,   ��  ��  ��      toggleData  ,INPUT plEnabled LOGICAL    ��  �  4�      showMessageProcedure    ,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL  �  x�  ��      returnFocus ,INPUT hTarget HANDLE   h�  ��  ��      repositionObject    ,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL    ��  ��  �      removeLink  ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE ��  \�  l�      removeAllLinks  ,   L�  ��  ��      modifyUserLinks ,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE p�  ��  ��      modifyListProperty  ,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER    ��  t�  ��      hideObject  ,   d�  ��  ��      exitObject  ,   ��  ��  ��      editInstanceProperties  ,   ��  ��  ��      displayLinks    ,   ��  �  �      createControls  ,   ��  (�  8�      changeCursor    ,INPUT pcCursor CHARACTER   �  d�  p�      applyEntry  ,INPUT pcField CHARACTER    T�  ��  ��      adjustTabOrder  ,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER ��  �  �      addMessage  ,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER ��  h�  p�      addLink ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE X�  ��  ��      unbindServer    ,INPUT pcMode CHARACTER ��  ��  �      runServerObject ,INPUT phAppService HANDLE  ��  8�  L�      disconnectObject    ,   (�  `�  p�      destroyObject   ,   P�  ��  ��      bindServer  ,   t�  ��  ��      transferDBRow   ,INPUT pcRowIdent CHARACTER,INPUT piRowNum INTEGER  ��  ��  �      startFilter ,   ��  �  (�      releaseDBRow    ,   �  <�  L�      refetchDBRow    ,INPUT phRowObjUpd HANDLE   ,�  x�  ��      filterContainerHandler  ,INPUT phFilterContainer HANDLE h�  ��  ��      fetchDBRowForUpdate ,   ��  ��  ��      confirmContinue ,INPUT-OUTPUT pioCancel LOGICAL ��  (�  8�      compareDBRow    ,   �  L�  `�      bufferCopyDBToRO    ,INPUT phRowObj HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER   <�  ��  ��      assignDBRow ,INPUT phRowObjUpd HANDLE   ��  �  �      updateState ,INPUT pcState CHARACTER    ��  D�  X�      updateQueryPosition ,   4�  l�  ��      updateAddQueryWhere ,INPUT pcWhere CHARACTER,INPUT pcField CHARACTER    \�  ��  ��      undoTransaction ,   ��  ��  ��      transferToExcel ,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT plUseExisting LOGICAL,INPUT piMaxRecords INTEGER  ��  x�  ��      synchronizeProperties   ,INPUT pcPropertiesForServer CHARACTER,OUTPUT pcPropertiesForClient CHARACTER   h�  ��  �      submitValidation    ,INPUT pcValueList CHARACTER,INPUT pcUpdColumns CHARACTER   ��  P�  d�      submitForeignKey    ,INPUT pcRowIdent CHARACTER,INPUT-OUTPUT pcValueList CHARACTER,INPUT-OUTPUT pcUpdColumns CHARACTER  @�  ��  ��      submitCommit    ,INPUT pcRowIdent CHARACTER,INPUT plReopen LOGICAL  ��  ,�  @�      startServerObject   ,   �  T�  d�      setPropertyList ,INPUT pcProperties CHARACTER   D�  ��  ��      serverFetchRowObjUpdTable   ,OUTPUT TABLE-HANDLE phRowObjUpd    ��  ��  ��      serverSendRows  ,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER,OUTPUT TABLE-HANDLE phRowObject    ��  ��  ��      saveContextAndDestroy   ,OUTPUT pcContext CHARACTER ��  ��   �      rowObjectState  ,INPUT pcState CHARACTER    ��  ,�  <�      retrieveFilter  ,   �  P�  d�      restartServerObject ,   @�  x�  ��      remoteSendRows  ,INPUT-OUTPUT piocContext CHARACTER,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT pioRowsReturned INTEGER,OUTPUT TABLE-HANDLE phRowObject,OUTPUT pocMessages CHARACTER   h�  ��  ��      refreshRow  ,   p�  ��  ��      printToCrystal  ,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT piMaxRecords INTEGER  ��  �  $�      isUpdatePending ,INPUT-OUTPUT plUpdate LOGICAL  �  T�  l�      initializeServerObject  ,   D�  ��  ��      initializeObject    ,   p�  ��  ��      home    ,   ��  ��  ��      genContextList  ,OUTPUT pcContext CHARACTER ��   �  �      fetchPrev   ,   ��   �  ,�      fetchNext   ,   �  @�  L�      fetchLast   ,   0�  `�  l�      fetchFirst  ,   P�  ��  ��      fetchBatch  ,INPUT plForwards LOGICAL   p�  ��  ��      endClientDataRequest    ,   ��  ��  ��      destroyServerObject ,   ��  �  �      describeSchema  ,INPUT pcSdoName CHARACTER,OUTPUT TABLE-HANDLE hTtSchema    ��  h�  x�      dataAvailable   ,INPUT pcRelative CHARACTER X�  ��  ��      copyColumns ,INPUT pcViewColList CHARACTER,INPUT phDataQuery HANDLE ��  ��  �      commitTransaction   ,   ��   �  0�      clientSendRows  ,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER    �  ��  ��      batchServices   ,INPUT pcServices CHARACTER,OUTPUT pcValues CHARACTER        � 
"     
 �%     adecomm/as-utils.w 
"   
   �    }        �
"     
    �     }        �� �  R   %               � 
" 	   
 %              h �P  \         (          
�                          
�            � �   �
" 	   
 -
�H T   %              �     }        �GG %              � 
"   
   P �L 
�H T   %              �     }        �GG %              
"   
   �        �    7%               
"   
 �               1�   
 �    %               o%   o           �     
"   
 �           �    1�    �    %               o%   o           � &   
"   
 �           �    1� -  
 �    %               o%   o           � 8   
"   
 �           l    1� H   �    %               o%   o           �     
"   
 �           �    1� V   �    %               o%   o           � e   
"   
 �           T    1� z   � �   %               o%   o           %               
"   
 �          �    1� �   � �     
"   
 �               1� �   �    %               o%   o           � �  
"   
 �           �    1� �   �    %               o%   o           � �  S 
"   
 �           �    1�    � �   %               o%   o           %               
"   
 �           p    1� -   � �   %               o%   o           %               
"   
 �           �    1� ?   � �   %               o%   o           %              
"   
 �          h    1� L   � �     
"   
 �           �    1� [  
 � �   %               o%   o           %               
"   
 �                1� f   �    %               o%   o           �     
"   
 �          �    1� n   � �     
"   
 �           �    1� ~   �    %               o%   o           � �  t 
"   
 �          D	    1� 	  
 � �     
"   
 �           �	    1�    �    %               o%   o           � %  � 
"   
 �           �	    1� �   �    %               o%   o           �     
"   
 �           h
    1� �  
 � �   %               o%   o           %               
"   
 .�           �
    1� �   .� �   %               o%   o           %              
"   
 -�           `    1� �   -�    %               o%   o           �     .
"   
 -�           �    1� �   -�    %               o%   o           o%   o           
"   
 -�           P    1�   
 -�    %               o%   o           �     .
"   
 -�           �    1�    -�   	 %               o%   o           � '  / -
"   
 �          8    1� W   �   	   
"   
 .�           t    1� i   .�   	 o%   o           o%   o           �     .
"   
 �          �    1� |   �   	   
"   
 -�           $    1� �   -�   	 o%   o           o%   o           �     -
"   
 �          �    1� �   � �     
"   
 �          �    1� �   �   	   
"   
 �              1� �   �   	   
"   
 �          L    1� �   �   	   
"   
 -�           �    1� �   -� �   o%   o           o%   o           %              
"   
 �              1� �   �   	   
"   
 �          @    1� �  
 � �     
"   
 �          |    1�    �   	   
"   
 �          �    1�    �   	   
"   
 �          �    1� %   �   	   
"   
 �          0    1� :   �   	   
"   
 �          l    1� I  	 �   	   
"   
 �          �    1� S   �   	   
"   
 �          �    1� f   �   	   
"   
 -�                1� }   -�    %               o%   o           o%   o           
�H T   %              �     }        �GG %              
"   
   
"   
 .
"   
   
"   
 �(�  L ( l       �        �    �� �   � P   �        �    �@    
� @  , 
�            �� �     p�               �L
�    %              � 8          � $         � �          
�    � �     
"   
 �� @  , 
�           �� -  
 �p�               �L"      P �L 
�H T   %              �     }        �GG %              
"   
 .�           �    1� �  
 .�    %               o%   o           �     .
"   
 .�           <    1� �  
 .�    %               o%   o           o%   o           
"   
 .�           �    1� �   .� �   %               o%   o           o%   o           
"   
 -�           4    1� �   -� �   %               o%   o           %               
"   
 .�           �    1� �   .� �   %               o%   o           %               
"   
 ��           ,    1� �   ��    %               o%   o           �     .
"   
 -�           �    1� �   -� �   %               o%   o           %              
"   
 -�               1� 
   -� �   %               o%   o           o%   o           
"   
 -�           �    1�    -�    %               o%   o           o%   o           
"   
 .�               1� $  	 .�    %               o%   o           �     .
"   
 .�           �    1� .   .�    %               o%   o           o%   o           
"   
 .�               1� B   .�    %               o%   o           o%   o           
"   
 .�           �    1� Q   .� �   %               o%   o           %               
"   
 .�           �    1� a   .� �   %               o%   o           o%   o           P �L 
�H T   %              �     }        �GG %              
"   
 .�           �    1� m  
 .� �   %               o%   o           %              
"   
 .�           H    1� x   .�    %               o%   o           o%   o           
"   
 .�           �    1� �   .�    %               o%   o           �     -
"   
 .�           8    1� �   .�    %               o%   o           o%   o           
"   
 �          �    1� �   � �     
"   
 -�           �    1� �   -�    %               o%   o           � �  ! .
"   
 -�           d    1� �   -�    %               o%   o           �     -
"   
 .�           �    1� �   .�    %               o%   o           �     -
"   
 �          L    1�    �      
"   
 �          �    1� "   � �     
"   
 .�           �    1� 6   .�    %               o%   o           �     .
"   
 �          8     1� B  
 � �     
"   
 ��           t     1� M   �� �   %               o%   o           o%   o           
"   
 -�           �     1� [   -� �   %               o%   o           %               
"   
 .�           l!    1� h   .� �   %               o%   o           %               
"   
 -�           �!    1� y   -�    %               o%   o           �     .
"   
 -�           \"    1� �   -�    %               o%   o           o%   o           
"   
 .�           �"    1� �   .� �   %               o%   o           %              
"   
 -�           T#    1� �   -� �   %               o%   o           %               
"   
 ��           �#    1� �   �� �   %               o%   o           %               
"   
 �          L$    1� �   � �     
"   
 �          �$    1� �   �      
"   
 -�           �$    1� �   -� �   %               o%   o           o%   o           
"   
 -�           @%    1� �   -�    %               o%   o           �     .
"   
 -�           �%    1� �   -�    %               o%   o           o%   o           
"   
 .�           0&    1�    .� �   o%   o           o%   o           o%   o           
"   
 .�           �&    1�    .�   	 %               o%   o           o%   o           
"   
 .�           ('    1� +   .�    %               o%   o           o%   o           
"   
 .�           �'    1� 8  
 .� �   %               o%   o           o%   o           
"   
 �           (    1� C   �      
"   
 -�           \(    1� T   -�    %               o%   o           � k  4 .
"   
 -�           �(    1� �  
 -� �   %               o%   o           %              
"   
 �          L)    1� �   � �     
"   
 .�           �)    1� �   .�    %               o%   o           �     �
"   
 .�           �)    1� �   .� �   %               o%   o           %              
"   
 -�           x*    1� �   -�    %               o%   o           �     .
"   
 .�           �*    1� �   .�    %               o%   o           �     -
"   
 .�           `+    1� �   .�    %               o%   o           �     .
"   
 .�           �+    1�     .� �   %               o%   o           %               
"   
 .�           P,    1�   	 .� �   %               o%   o           o%   o           
"   
 ��           �,    1�    ��    %               o%   o           � (  	 -
"   
 .�           @-    1� 2   .� �   %               o%   o           %       �       
"   
 .�           �-    1� >   .�    %               o%   o           �     .
"   
 .�           0.    1� E   .� �   o%   o           o%   o           %              
"   
 .�           �.    1� W   .� �   %               o%   o           %               
"   
 .�           (/    1� n   .�    %               o%   o           o%   o           
"   
 .�           �/    1�    .�   	 %               o%   o           �     .
"   
 �          0    1� �   �   	   P �L 
�H T   %              �     }        �GG %              
"   
 ��           �0    1� �  
 ��    %               o%   o           �     �
"   
 .�           1    1� �   .� �   %               o%   o           %               
"   
 .�           �1    1� �  	 .�    %               o%   o           �     .
"   
 -�           2    1� �   -�    %               o%   o           �     .
"   
 .�           �2    1� �   .� �   %               o%   o           %               
"   
 .�           �2    1� �   .�    %               o%   o           �     .
"   
 .�           p3    1� �   .�    %               o%   o           o%   o           
"   
 .�           �3    1� �   .�    %               o%   o           o%   o           
"   
 -�           h4    1�    -� �   %               o%   o           o%   o           
"   
 ��           �4    1�    �� �   %               o%   o           o%   o           
"   
 .�           `5    1� #   .� �   %               o%   o           o%   o           
"   
 .�           �5    1� 4   .�    %               o%   o           o%   o           
"   
 .�           X6    1� C  	 .�   	 %               o%   o           �     -
"   
 -�           �6    1� M  
 -�   	 %               o%   o           �     .
"   
 .�           @7    1� X   .�    %               o%   o           �     -
"   
 .�           �7    1� g   .�    %               o%   o           o%   o           
"   
 .�           08    1� u   .�    %               o%   o           o%   o           
"   
 .�           �8    1� �   .�    %               o%   o           �     .
"   
 .�            9    1� �   .�    %               o%   o           �     .
"   
 .�           �9    1� �   .�   	 %               o%   o           o%   o           
"   
 �          :    1� �   � �     
"   
 -�           L:    1� �   -�    %               o%   o           �     .
"   
 -�           �:    1� �   -�    %               o%   o           o%   o           
"   
 ��           <;    1� �   �� �   %               o%   o           o%   o           
"   
 .�           �;    1� �  
 .�    %               o%   o           �     .
"   
 .�           ,<    1�    .�    %               o%   o           �     .
"   
 -�           �<    1�    -� �   %               o%   o           %               P �L 
�H T   %              �     }        �GG %              
"   
 -�           p=    1� +  	 -� �   %               o%   o           o%   o           
"   
 -�           �=    1� 5   -� �   %               o%   o           o%   o           
"   
 .�           h>    1� D   .� �   %               o%   o           o%   o           
"   
 ��           �>    1� S   �� �   %               o%   o           %              
"   
 .�           `?    1� g   .�    %               o%   o           � �  M �
"   
 .�           �?    1� �   .� �   %               o%   o           %              
"   
 .�           P@    1� �   .� �   %               o%   o           %               
"   
 .�           �@    1� �   .� �   %               o%   o           %               
"   
 .�           HA    1� 
   .�   	 %               o%   o           �    .
"   
 .�           �A    1� 5   .� �   %               o%   o           %               
"   
 .�           8B    1� D   .�   	 %               o%   o           o%   o           
"   
 .�           �B    1� Q   .� �   o%   o           o%   o           %              
"   
 ��           0C    1� a   ��   	 o%   o           o%   o           �     �
"   
 .�           �C    1� t   .� �   o%   o           o%   o           o%   o           
"   
 .�            D    1� �   .� �   o%   o           o%   o           o%   o           
"   
 .�           �D    1� �   .�   	 o%   o           o%   o           o%   o           
"   
 .�           E    1� �   .� �   o%   o           o%   o           o%   o           
"   
 .�           �E    1� �   .�   	 o%   o           o%   o           � �   .
"   
 .�           F    1� �   .�   	 o%   o           o%   o           � �   .
"   
 .�           |F    1� �   .� �   %               o%   o           %               
"   
 -�           �F    1� �   -� �   %               o%   o           %               
"   
 �          tG    1�    �   	   
"   
 -�           �G    1�    -� �   %               o%   o           %               
"   
 -�           ,H    1� &   -�    %               o%   o           o%   o           
"   
 .�           �H    1� :   .�    %               o%   o           o%   o           
"   
 .�           $I    1� N   .� �   %               o%   o           o%   o           
"   
 .�           �I    1� `   .�    %               o%   o           �     .
"   
 .�           J    1� o   .� }   %               o%   o           %               
"   
 -�           �J    1� �  	 -� �   %               o%   o           %                "    %     start-super-proc �%     adm2/smart.p 7�P �L 
�H T   %              �     }        �GG %              
"   
   �       �K    6� �     
"   
   
�        �K    8
"   
   �        �K    ��     }        �G 4              
"   
 ߱G %              G %              %�   AppService,ASInfo,ASUsePrompt,CacheDuration,CheckCurrentChanged,DestroyStateless,DisconnectAppServer,ServerOperatingMode,ShareData,UpdateFromSource,ForeignFields,ObjectName,OpenOnInit,PromptColumns,PromptOnDelete,RowsToBatch,RebuildOnRepos,ToggleDataTargets  
�H T   %              �     }        �GG %              
"   
 �
"   
 
"   
 �
"   
   (�  L ( l       �        �M    �� �   � P   �        �M    �@    
� @  , 
�       �M    �� �   �p�               �L
�    %              � 8       N    � $         � �          
�    � �   �
"   
 �p� @  , 
�       O    �� �   �p�               �L"  	  , �   � �   .� �   �     }        �A      |    "  	    � �   .%              (<   \ (    |    �     }        �A� �   �A"  
  .    "  	  �"  
  .  < "  	  �"  
  .(    |    �     }        �A� �   �A"  
  .
�H T   %              �     }        �GG %              
"   
 �
"   
 
"   
 �
"   
   (�  L ( l       �        �P    �� �   � P   �        �P    �@    
� @  , 
�       �P    �� �   �p�               �L
�    %              � 8      Q    � $         � �          
�    � �   �
"   
 �p� @  , 
�       R    ��   
 �p�               �L"  	  , 
�H T   %              �     }        �GG %              
"   
   
"   
 /
"   
   
"   
   (�  L ( l       �        �R    �� �   � P   �        �R    �@    
� @  , 
�       �R    �� �     p�               �L
�    %              � 8      �R    � $         � �          
�    � �     
"   
 �p� @  , 
�       �S    �� -  
 �p�               �L%     SmartDataObject 
"   
   p� @  , 
�       XT    �� H     p�               �L%               
"   
  p� @  , 
�       �T    �� �    p�               �L%               
"   
  p� @  , 
�       U    �� i    p�               �L(        �       �       �       �     }        �A
�H T   %              �     }        �GG %              
"   
 . (   � 
"   
 �    �        �U    �� �   �
"   
   � 8      DV    � $         � �          
�    � �   �
"   
   �        �V    �
"   
   �       �V    /
"   
   
"   
   �       �V    6� �     
"   
   
�        W    8
"   
   �        4W    �
"   
   �       TW    �
"   
   p�    � �   .
�    �     }        �G 4              
"   
 ߱G %              G %              
�     }        �
"   
    (   � 
"   
 �    �        X    �A"    �A
"   
   
�        dX    �@ � 
"   
 ."      �       }        �
"   
 %              %                "    %     start-super-proc �%     adm2/appserver.p V.�    � i     
�    �     }        �%               %      Server  - �     }        �    "    -�     %               %      Client      "    -�     %      NONE    p�,  8         $     "    .        � �   �
�    
�H T   %              �     }        �GG %              
"   
 �
"   
 
"   
 �
"   
   (�  L ( l       �        �Z    �� �   � P   �        �Z    �@    
� @  , 
�       �Z    �� �   �p�               �L
�    %              � 8      �Z    � $         � �          
�    � �   �
"   
 �p� @  , 
�       �[    �� .   �p�               �L"    , p�,  8         $     "    .        � �   �
�     "    %     start-super-proc �%     adm2/dataquery.p �.
�H T   %              �     }        �GG %              
"   
 �
"   
 
"   
 �
"   
 �(�  L ( l       �         ]    �� �   � P   �        ,]    �@    
� @  , 
�       8]    �� �   �p�               �L
�    %              � 8      D]    � $         � �   �     
�    � �   �
"   
 �p� @  , 
�       T^    ��    �p�               �L%H > 8   dataAvailable,confirmContinue,isUpdatePending,buildDataRequest  
�H T   %              �     }        �GG %              
"   
 �
"   
 
"   
 �
"   
 �(�  L ( l       �        8_    �� �   � P   �        D_    �@    
� @  , 
�       P_    �� �   �p�               �L
�    %              � 8      \_    � $         � �   �     
�    � �   �
"   
 �p� @  , 
�       l`    �� z   �p�               �L%               "    %     start-super-proc �%     adm2/query.p 7�%     start-super-proc �%     adm2/queryext.p % 	    initProps �
�    %d Z T   FOR EACH ttCcbDDocu NO-LOCK,       FIRST Almmmatg OF ttCcbDDocu NO-LOCK INDEXED-REPOSITION  �   � 4     � 6     � 8         "    .� L    
�H T   %              �     }        �GG %              
"   
 �
"   
 
"   
 �
"   
   (�  L ( l       �        tb    �� �   � P   �        �b    �@    
� @  , 
�       �b    �� �   �p�               �L
�    %              � 8      �b    � $         � �          
�    � �   �
"   
 �p� @  , 
�       �c    �� �  	 �p�               �L"    , %               �    "      � 6         %              %                   "      %                  "      "      "     T(        "    .%              "    .� 6   "      �       "    ��    "    .� �   �       � �   ��    "     � �    S    "      "        "    .%                � @    �     t T     P   4       "      (0       4       ."      �       �     �� 4   .T ,  %              T   "    ."    � 6     � �   �� 4   .T    �    "    .� �   "      � �   �"      %                   %              %                   "      %                  "      �     "      �     "       \      H   "      ((       "    �%              �     � M     4  /     "      
�H T   %              �     }        �GG %              
"   
 �
"   
 
"   
 �
"   
   (�  L ( l       �        ph    �� �   � P   �        |h    �@    
� @  , 
�       �h    �� �   �p�               �L
�    %              � 8      �h    � $         � �          
�    � �   �
"   
 �p� @  , 
�       �i    �� �  
 �p�               �L"    ,       "  
  .�    �     -� 6         "  	    �    � O  " � 6   -�   � 4     � 6     �     ��   � 4     � 6   �� O  " -      "  
  -�    �     .� 6         "  	    �    � r   � 6   .   ,        "    �� M   .�   � 4   �� 6   .�        ,        "      � M     �   � 4   .� 6   � r   -�   � 4     � 6     � �  0   
�H T   %              �     }        �GG %              
"   
 
"   
 �
"   
 
"   
 (�  L ( l       �        Dl    �� �   � P   �        Pl    �@    
� @  , 
�       \l    �� �   p�               �L
�    %              � 8      hl    � $         � �          
�    � �     
"   
 �p� @  , 
�       xm    �� 6   �p�               �L"    , 
"   
   p� @  , 
�       �m    ��      p�               �L"    , 
"   
  p� @  , 
�       (n    �� �    p�               �L"    ,     %              %                   "      %                  "      �     "      �     "      4 (        "  
    �    �       � 6         "  	  /�     "    -T    "      "      @ A,    �   � 4   � M     "    �"       T      @   "    (        "      �     ��       � 4   �"    .     "  	   %              D H   @ A,    �   � 4   �� M     "    �"    .,    S   "    ��     .� 6   %                T      @   "    (        "      �     ��       � 4   �"    .     "  
   %                         "    � M     "    �           "      � M   �"      
�H T   %              �     }        �GG %              
"   
 .
"   
   
"   
 .
"   
 �(�  L ( l       �        Dr    �� �   � P   �        Pr    �@    
� @  , 
�       \r    �� �   .p�               �L
�    %              � 8      hr    � $         � �   �     
�    � �   
"   
 �p� @  , 
�       xs    ��    �p�               �L"    , 
"   
   p� @  , 
�       �s    �� �     p�               �L"    , "      %              %              %              %              %              %              %              %              %       	       %       
       %              %              %              %              %              %              %              %              "       "    %     start-super-proc �%     adm2/data.p %     start-super-proc �%     adm2/dataext.p %     start-super-proc �%     adm2/dataextcols.p %     start-super-proc �%     adm2/dataextapi.p .
�H T   %              �     }        �GG %              
"   
 �
"   
 
"   
 �
"   
 �(�  L ( l       �        �v    �� �   � P   �        w    �@    
� @  , 
�       w    �� �   �p�               �L
�    %              � 8      w    � $         � �   �     
�    � �   �
"   
 �p� @  , 
�       ,x    ��    �p�               �L%               %(     "ccb/dt-nota-cr-db-detail.i" 7�
�H T   %              �     }        �GG %              
"   
 �
"   
 
"   
 �
"   
   (�  L ( l       �        y    �� �   � P   �        y    �@    
� @  , 
�       y    �� �   �p�               �L
�    %              � 8      (y    � $         � �          
�    � �   �
"   
 �p� @  , 
�       8z    �� 
   �p�               �L"    , 
�     	         �G
�H T   %              �     }        �GG %              
"   
 �
"   
 
"   
 �
"   
   (�  L ( l       �        �z    �� �   � P   �        �z    �@    
� @  , 
�       {    �� �   �p�               �L
�    %              � 8      {    � $         � �          
�    � �   �
"   
 �p� @  , 
�       $|    �� B  
 �p�               �L
"   
 , 
�     
        �G
�H T   %              �     }        �GG %              
"   
 �
"   
 
"   
 �
"   
   (�  L ( l       �        �|    �� �   � P   �        �|    �@    
� @  , 
�       �|    �� �   �p�               �L
�    %              � 8       }    � $         � �          
�    � �   �
"   
 �p� @  , 
�       ~    ��   	 �p�               �L
"   
 , 
"   
      � �   	   �        h~    �
�H T   %              �     }        �GG %              
"   
 �
"   
 
"   
 �
"   
   (�  L ( l       �        �~    �� �   � P   �        �~    �@    
� @  , 
�            �� �   �p�               �L
�    %              � 8          � $         � �          
�    � �   �
"   
 �p� @  , 
�       �    �� y   �p�               �L"    , 
"   
   �       t�    �"      
�     
        �G
�H T   %              �     }        �GG %              
"   
 �
"   
 
"   
 �
"   
   (�  L ( l       �         �    �� �   � P   �        �    �@    
� @  , 
�       �    �� �   �p�               �L
�    %              � 8      $�    � $         � �          
�    � �   �
"   
 �p� @  , 
�       4�    �� +  	 �p�               �L
"   
 , 
�             �Gp�,  8         $     
"   
 .        � �    �
�    
�             �Gp�,  8         $     
"   
 .        � �    �
�    �    � �      
�        "    .�     %     modifyListProperty 
�    %      REMOVE  %     SupportedLinks %     Update-Target  %     bufferValidate  
�    "      �  %      setContextAndInitialize 
�    "      %     bufferCommit    
�    "      "      �    � G!     
�    %               %     bufferCommit    
�    "      "      
�     
        �G�     }        �
�                    �           �   l       ��                 m  �  �               �C.                    O   ����    e�          O   ����    R�          O   ����    ��        $  |  �   ���                       �U     
                    � ߱              }  (  �      V      4   ����V                �                      ��                  ~  �                  �/                       ~  8  �  �    PV            �  �  `      �V      4   �����V                p                      ��                  �  �                  (/                       �  �  �  o   �      ,                                 �  �   �  �V      �  �   �  �V      $  $  �  �  ���                        W     
                    � ߱        8  �   �  @W      L  �   �  `W      `  �   �  �W          $   �  �  ���                       �W  @         �W              � ߱                     T          ,  @   T �            
             
             
             
                 $   4   D          $   4   D   ����        ��                            ����                                            �           �   l       ��                 �  �  �               P�.                    O   ����    e�          O   ����    R�          O   ����    ��      	                      �          �  $  �    ���                       X     
                    � ߱                  �  �                      ��                   �  �                  p�.                     �  4      4   ����$X      $  �  �  ���                       pX     
                    � ߱        �    �  4  D      �X      4   �����X      /  �  p                               3   �����X  �  �   �  �X          O   �  ��  ��  �X                               , �                          
                               �      ��                            ����                                            �           �   l       ��                 \  �  �                b.                    O   ����    e�          O   ����    R�          O   ����    ��        $  |  �   ���                       ta                         � ߱        d  $  }  8  ���                       �a                         � ߱             �  �  �      b      4   ����b  (b     
                �b                     �c  @        
 �c              � ߱            V   �  �  ���                        x  $    L  ���                        d                         � ߱        <  $    �  ���                       d                         � ߱          L      �  �                      ��        0                             t
/      �d     X       �      $    x  ���                       4d                         � ߱        �  $    �  ���                       dd                         � ߱            4   �����d  �d                      e                     e                     \e                     |e                         � ߱        �  $      ���                               �         �e      4   �����e      $    ,  ���                       �e          �f             � ߱          $     �  ���                       �f                         � ߱          ,      �  �                      ��        0         "  '                  �/      �g     �     "  �      $  "  X  ���                       g                         � ߱        �  $  "  �  ���                       @g                         � ߱            4   ����hg      $  $    ���                       �g                         � ߱        $h     
                �h                     �i  @        
 �i              � ߱        T  V   2  D  ���                        �i       
       
       0j       	       	       dj                     �j                         � ߱         	  $  y  �  ���                       �j       
       
       �j       	       	       $k                     xk                         � ߱        ,	  $  �  �  ���                        
  $    X	  ���                       �k                         � ߱        �k     
                tl                     �m  @        
 �m          n  @        
 �m          tn  @        
 4n              � ߱        �
  V   )  �	  ���                          �
         �                      ��        0         �  �                  ��.       o     p     �  L
      $  �  �
  ���                       �n                         � ߱        x  $  �  L  ���                       �n                         � ߱        �  4   �����n      4   ����o  �  $  �  �  ���                       xo                         � ߱            �    �      �o      4   �����o                �                      ��                  �  �                  ��.                       �    �o                     Dp       	       	           � ߱            $  �  �  ���                             �  (  �      lp      4   ����lp                �                      ��                  �  �                  �.                       �  8   q                     hq       
       
           � ߱            $  �  �  ���                       �q                     �q                         � ߱          $  �  (  ���                       �q     
                tr                     �s  @        
 �s          t  @        
 �s              � ߱            V   �  �  ���                                    7           �  �  � |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             �   �   �   �   �   �   �   �       $  4  D  T  d  t  �  \  �  �  �  �  8HXhx��������(8HX      �   �   �   �   �   �   �   �      $  4  D  T  d  t  �   \ �  �  �  �  8HXhx��������(8HX  �                    � �                     �          ��                            ����                                                        �   l       ��                  g  r  �               �M/                    O   ����    e�          O   ����    R�          O   ����    ��              !                    ��                            ����                                            �           �   l       ��                  |  �  �               hN/                    O   ����    e�          O   ����    R�          O   ����    ��      	!       �              �                  $                  d  /  �  $     4   �                      3   �����            T                      3   �����      O   �  ��  ��  �               �          �  �    �                                             ��                            ����                                            H          �   l       ��                  �  �  �               LO/                    O   ����    e�          O   ����    R�          O   ����    ��      (!       �              �                $                  2!       ,             �          =!                                �  /  �  t     �  <�                      3   �����            �                      3   ����D�     /  �  �     �  l�                      3   ����P�  x                             3   ����t�      $   �  L  ���                                                   � ߱                  �  �                  3   ������      $   �  �  ���                                                   � ߱        X  $  �  ,  ���                       ��                         � ߱            O   �  ��  ��  ��               �          �  �   @ �                                                              0              0           ��                            ����                                                       �   l       ��                  �  �  �               4)/                    O   ����    e�          O   ����    R�          O   ����    ��      �       $                  2!                    �          =!                      �              /  �  L     \  ؄                      3   ������  �        |  �                  3   ������      $   �  �  ���                                                   � ߱                                      3   �����      $   �  @  ���                                                   � ߱                     �          �  �   , �                                                                 ��                            ����                                            �           �   l       ��                  I  T  �               �{/                    O   ����    e�          O   ����    R�          O   ����    ��            S  �   �       �      4   �����      �   S   �    ��                            ����                            TXS appSrvUtils ttCcbDDocu CodCia CodDoc NroDoc NroItm UndVta codmat PreUni PorDto PorDto2 ImpDto ImpLin CanDes Pesmat AftIgv AftIsc PreBas PreVta ImpIgv ImpIsc Factor CanDev CodCli AlmDes Por_Dsctos Flg_Factor CodDiv FchDoc ImpCto puntos mrguti ImpPro ImpDto2 PorDcto_Adelanto ImpDcto_Adelanto Dcto_Otros_Mot Dcto_Otros_Factor Dcto_Otros_VV Dcto_Otros_PV cTipoAfectacion cPreUniSinImpuesto FactorDescuento TasaIGV ImporteUnitarioSinImpuesto ImporteReferencial ImporteBaseDescuento ImporteDescuento ImporteTotalSinImpuesto MontoBaseIGV ImporteIGV ImporteTotalImpuestos ImporteUnitarioConImpuesto cImporteVentaExonerado cImporteVentaGratuito cSumaImpteTotalSinImpuesto cMontoBaseIGV cSumaIGV cOtrosTributosOpGratuito ImpuestoBolsaPlastico MontoTributoBolsaPlastico CantidadBolsaPlastico MontoUnitarioBolsaPlastico cImporteTotalConImpuesto ImporteBaseDescuentoNoAfecto FactorDescuentoNoAfecto ImporteDescuentoNoAfecto D:\newsie\on_in_co\aplic\ccb\dt-nota-cr-db-detail.w should only be RUN PERSISTENT. ADDROW CANCELROW CANNAVIGATE CLOSEQUERY COLUMNPROPS COLVALUES COPYROW CREATEROW DELETEROW FETCHROW FETCHROWIDENT FINDROW FINDROWWHERE FIRSTROWIDS GETLASTCOMMITERRORTYPE HASFOREIGNKEYCHANGED OPENDATAQUERY OPENQUERY PREPAREQUERY ROWAVAILABLE ROWVALUES SUBMITROW UPDATEROW GETOBJECTTYPE xiRocketIndexLimit ADDQUERYWHERE ASSIGNQUERYSELECTION BUFFERCOMPAREDBTORO BUFFERWHERECLAUSE COLUMNDATATYPE COLUMNDBCOLUMN COLUMNQUERYSELECTION COLUMNTABLE COLUMNVALMSG DBCOLUMNDATANAME DBCOLUMNHANDLE EXCLUDECOLUMNS GETDATACOLUMNS GETFOREIGNVALUES GETQUERYPOSITION GETQUERYSORT GETQUERYSTRING GETQUERYWHERE GETTARGETPROCEDURE INDEXINFORMATION INSERTEXPRESSION NEWQUERYSTRING NEWQUERYVALIDATE NEWQUERYWHERE NEWWHERECLAUSE REFRESHROWIDENT REMOVEFOREIGNKEY REMOVEQUERYSELECTION RESOLVEBUFFER ROWIDWHERE ROWIDWHERECOLS SETQUERYPOSITION SETQUERYSORT SETQUERYSTRING SETQUERYWHERE WHERECLAUSEBUFFER GETAPPSERVICE GETASBOUND GETASDIVISION GETASHANDLE GETASHASSTARTED GETASINFO GETASINITIALIZEONRUN GETASUSEPROMPT GETSERVERFILENAME GETSERVEROPERATINGMODE RUNSERVERPROCEDURE SETAPPSERVICE SETASDIVISION SETASHANDLE SETASINFO SETASINITIALIZEONRUN SETASUSEPROMPT SETSERVERFILENAME SETSERVEROPERATINGMODE gshAstraAppserver gshSessionManager gshRIManager gshSecurityManager gshProfileManager gshRepositoryManager gshTranslationManager gshWebManager gscSessionId gsdSessionObj gshFinManager gshGenManager gshAgnManager gsdTempUniqueID gsdUserObj gsdRenderTypeObj gsdSessionScopeObj ghProp ghADMProps ghADMPropsBuf glADMLoadFromRepos glADMOk ANYMESSAGE ASSIGNLINKPROPERTY FETCHMESSAGES GETCHILDDATAKEY GETCONTAINERHANDLE GETCONTAINERHIDDEN GETCONTAINERSOURCE GETCONTAINERSOURCEEVENTS GETCONTAINERTYPE GETDATALINKSENABLED GETDATASOURCE GETDATASOURCEEVENTS GETDATASOURCENAMES GETDATATARGET GETDATATARGETEVENTS GETDBAWARE GETDESIGNDATAOBJECT GETDYNAMICOBJECT GETINSTANCEPROPERTIES GETLOGICALOBJECTNAME GETLOGICALVERSION GETOBJECTHIDDEN GETOBJECTINITIALIZED GETOBJECTNAME GETOBJECTPAGE GETOBJECTPARENT GETOBJECTVERSION GETOBJECTVERSIONNUMBER GETPARENTDATAKEY GETPASSTHROUGHLINKS GETPHYSICALOBJECTNAME GETPHYSICALVERSION GETPROPERTYDIALOG GETQUERYOBJECT GETRUNATTRIBUTE GETSUPPORTEDLINKS GETTRANSLATABLEPROPERTIES GETUIBMODE GETUSERPROPERTY INSTANCEPROPERTYLIST LINKHANDLES LINKPROPERTY MAPPEDENTRY MESSAGENUMBER PROPERTYTYPE REVIEWMESSAGES SETCHILDDATAKEY SETCONTAINERHIDDEN SETCONTAINERSOURCE SETCONTAINERSOURCEEVENTS SETDATALINKSENABLED SETDATASOURCE SETDATASOURCEEVENTS SETDATASOURCENAMES SETDATATARGET SETDATATARGETEVENTS SETDBAWARE SETDESIGNDATAOBJECT SETDYNAMICOBJECT SETINSTANCEPROPERTIES SETLOGICALOBJECTNAME SETLOGICALVERSION SETOBJECTNAME SETOBJECTPARENT SETOBJECTVERSION SETPARENTDATAKEY SETPASSTHROUGHLINKS SETPHYSICALOBJECTNAME SETPHYSICALVERSION SETRUNATTRIBUTE SETSUPPORTEDLINKS SETTRANSLATABLEPROPERTIES SETUIBMODE SETUSERPROPERTY SHOWMESSAGE SIGNATURE prepareInstance ObjectName CHAR  ObjectVersion ADM2.2 ObjectType SmartDataObject ContainerType PropertyDialog adm2/support/datad.w QueryObject LOGICAL ContainerHandle HANDLE InstanceProperties AppService,ASInfo,ASUsePrompt,CacheDuration,CheckCurrentChanged,DestroyStateless,DisconnectAppServer,ServerOperatingMode,ShareData,UpdateFromSource,ForeignFields,ObjectName,OpenOnInit,PromptColumns,PromptOnDelete,RowsToBatch,RebuildOnRepos,ToggleDataTargets SupportedLinks Data-Source,Data-Target,Navigation-Target,Update-Target,Commit-Target,Filter-Target ContainerHidden ObjectInitialized ObjectHidden ObjectsCreated HideOnInit UIBMode ContainerSource ContainerSourceEvents initializeObject,hideObject,viewObject,destroyObject,enableObject,confirmExit,confirmCancel,confirmOk,isUpdateActive DataSource DataSourceEvents dataAvailable,queryPosition,updateState,deleteComplete,fetchDataSet,confirmContinue,confirmCommit,confirmUndo,assignMaxGuess,isUpdatePending TranslatableProperties ObjectPage INT DBAware DesignDataObject DataSourceNames DataTarget DataTargetEvents CHARACTER updateState,rowObjectState,fetchBatch,LinkState LogicalObjectName PhysicalObjectName LogicalVersion PhysicalVersion DynamicObject RunAttribute ChildDataKey ParentDataKey DataLinksEnabled InactiveLinks InstanceId DECIMAL SuperProcedure SuperProcedureMode SuperProcedureHandle LayoutPosition ClassName RenderingProcedure ThinRenderingProcedure Label cType ADMProps Target WHERE Target = WIDGET-H(" ") AppService ASDivision ASHandle ASHasConnected ASHasStarted ASInfo ASInitializeOnRun ASUsePrompt BindSignature BindScope ServerOperatingMode ServerFileName ServerFirstCall NeedContext AutoCommit BLOBColumns BufferHandles CLOBColumns CommitSource CommitSourceEvents commitTransaction,undoTransaction CommitTarget CommitTargetEvents rowObjectState CurrentRowid ROWID CurrentUpdateSource DataColumns DataHandle DataIsFetched DataModified DataQueryBrowsed DataQueryString FetchOnOpen FillBatchOnRepos FilterActive FilterAvailable FilterSource FilterWindow FirstRowNum ForeignFields ForeignValues IgnoreTreeViewFilter IndexInformation LargeColumns LastRowNum NavigationSource NavigationSourceEvents fetchFirst,fetchNext,fetchPrev,fetchLast,startFilter OpenOnInit PrimarySDOSource PromptColumns PromptOnDelete QueryColumns QueryPosition QueryString RebuildOnRepos RowObject RowObjectState NoUpdates RowsToBatch Tables ToggleDataTargets TransferChildrenForAll UpdatableColumns UpdatableWhenNew UpdateSource AssignList AuditEnabled BaseQuery CalcFieldList CheckLastOnOpen DataColumnsByTable DBNames EntityFields FetchHasAudit FetchHasComment FetchAutoComment FirstResultRow KeyFields KeyTableId LastDBRowIdent LastResultRow NewBatchInfo NoLockReadOnlyTables PhysicalTables PositionForClient QueryHandle QueryRowIdent RequiredProperties SkipTransferDBRow TempTables UpdatableColumnsByTable UpdateFromSource RowObjUpd RowObjectTable RowObjUpdTable CheckCurrentChanged StatelessSavedProperties CheckCurrentChanged,RowObjectState,LastResultRow,FirstResultRow,QueryRowIdent DestroyStateless DisconnectAppServer ServerSubmitValidation DataFieldDefs "ccb/dt-nota-cr-db-detail.i" QueryContainer QueryContext AsynchronousSDO DataLogicProcedure DataLogicObject DataReadHandler DataReadColumns DataReadBuffer DataDelimiter | DataReadFormat TrimNumeric IsRowObjectExternal IsRowObjUpdExternal ManualAddQueryWhere DynamicData LastCommitErrorType LastCommitErrorKeys RunDataLogicProxy SchemaLocation CacheDuration INTEGER ShareData ghContainer adm2/smart.p cObjectName iStart / \ . hReposBuffer hPropTable hBuffer hTable deleteProperties ADM-CLONE-PROPS pcProcName hProc START-SUPER-PROC cAppService cASDivision cServerOperatingMode adm2/appserver.p getAppService Server Client NONE setASDivision setAppService adm2/dataquery.p dataAvailable,confirmContinue,isUpdatePending,buildDataRequest adm2/query.p adm2/queryext.p cTable iTable cColumns cDataCols cUpdCols cCalcData cCalcUpd iNumData iNumUpd cBuffers cKeyFields cAssignList iAssigns iPos iEntry iCount cTables cTableAssign cDbEntry cField cKeyTable cQueryString FOR EACH ttCcbDDocu NO-LOCK,       FIRST Almmmatg OF ttCcbDDocu NO-LOCK INDEXED-REPOSITION ,   ttCcbDDocu Almmmatg  ; NroItm codmat UndVta CanDes ImpLin DesMat DesMar NroItm codmat DesMat DesMar UndVta CanDes ImpLin INITPROPS piTableIndex lRet DELETERECORDSTATIC adm2/data.p adm2/dataext.p adm2/dataextcols.p adm2/dataextapi.p DesMat DesMar RowNum RowIdent RowMod RowIdentIdx RowUserProp ChangedFields cContainerType hRowObject hDataQuery cDataFieldDefs FOR EACH  setRowObjectTable setRowObjUpdTable getUpdatableColumns REMOVE Update-Target PUSHROWOBJUPDTABLE pcValType PUSHTABLEANDVALIDATE pcContext pcMessages pcUndoIds obtainContextForClient REMOTECOMMIT SERVERCOMMIT GETROWOBJUPDSTATIC DISABLE_UI llave01 llave02 llave03 llave04 qDataQuery x   5  �  �B      / �<   ��      0         pcServices      ��      T         pcServices  �   ��      x         piStartRow  �   ��      �         piStartRow  �   ��      �         piStartRow  �   ��      �         piStartRow      ��              piStartRow  <  ��      ,        pcViewColList       ��      T       
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
 hTarget �  ��      �        pcMessage       ��      �        pcMessage       ��      �        plEnabled             �     cType       0     V   �                             getObjectType   _	  w	  y	  `        P  
   hReposBuffer    �        t  
   hPropTable  �        �  
   hBuffer           �  
   hTable  �  �     W   <          �                  adm-clone-props |  }  ~    �  �  �  �  �  �  �  �  �  �  �  �  �            P  
   hProc             p        pcProcName  �  �  	   X   <  X      �                  start-super-proc    �  �  �  �  �  �  �  �  �           �     cTable               iTable  <        0     cColumns    \        P     cDataCols   |        p     cUpdCols    �        �     cCalcData   �        �     cCalcUpd    �     	   �     iNumData    �     
   �     iNumUpd              cBuffers    8        ,     cKeyFields  X        L     cAssignList x        l     iAssigns    �        �     iPos    �        �     iEntry  �        �     iCount  �        �     cTables        �     cTableAssign    ,              cDbEntry    H        @     cField  h        \     cKeyTable             |     cQueryString    |  �  "   Y   �          �                  initProps   |  }  �  �                   "  $  '  2  y  �    )  �  �  �  �  �  �  �  �  �  �  �  �  �  �            d     lRet              �        piTableIndex    �  �  (   Z   P  l      �                  deleteRecordStatic      -  .  J  K  g  h  �  �  �  �  �  �  �  �  �  �      2  3  O  P  l  m  �  �  �  �  �  �  �  �  �  �                         !       �  �     [       x      �                  pushRowObjUpdTable  r  �        �        pcValType                  $       �  \     \       �      D                  pushTableAndValidate    �  �  �  �        �        pcContext   �             $       �        �        pcMessages            �        pcUndoIds     ,     ]       h                        remoteCommit    �  �  �  �  �  X             $       |        p        pcMessages            �        pcUndoIds   �  �     ^       @      �                  serverCommit    �  �  �  ,     _                                 getRowObjUpdStatic      �  p     `               d                  disable_UI  S  T  4  H&       �       �%                      �  �  �  A   ttCcbDDocu  �         �         �         �         �         �                                                        (         0         8         @        H         P         X         `         h         p         x         �         �        �         �         �         �         �         �         �         �         �        �                           $         4         D         T         h         x         �         �         �         �         �         �                           (         D         \         t         �         �         �         �         �         �                  0         L         l         �         CodCia  CodDoc  NroDoc  NroItm  UndVta  codmat  PreUni  PorDto  ImpDto  ImpLin  CanDes  AftIgv  AftIsc  PreBas  PreVta  ImpIgv  ImpIsc  Factor  CanDev  PorDto2 Pesmat  CodCli  AlmDes  Por_Dsctos  Flg_Factor  FchDoc  CodDiv  ImpCto  puntos  mrguti  ImpPro  ImpDto2 PorDcto_Adelanto    ImpDcto_Adelanto    Dcto_Otros_Mot  Dcto_Otros_Factor   Dcto_Otros_VV   Dcto_Otros_PV   cTipoAfectacion cPreUniSinImpuesto  FactorDescuento TasaIGV ImporteUnitarioSinImpuesto  ImporteReferencial  ImporteBaseDescuento    ImporteDescuento    ImporteTotalSinImpuesto MontoBaseIGV    ImporteIGV  ImporteTotalImpuestos   ImporteUnitarioConImpuesto  cImporteVentaExonerado  cImporteVentaGratuito   cSumaImpteTotalSinImpuesto  cMontoBaseIGV   cSumaIGV    cOtrosTributosOpGratuito    ImpuestoBolsaPlastico   MontoTributoBolsaPlastico   CantidadBolsaPlastico   MontoUnitarioBolsaPlastico  cImporteTotalConImpuesto    ImporteBaseDescuentoNoAfecto    FactorDescuentoNoAfecto ImporteDescuentoNoAfecto    �  �  �     RowObject   L         T         \         d         l         t         |         �         �         �         �         �         NroItm  codmat  DesMat  DesMar  UndVta  CanDes  ImpLin  RowNum  RowIdent    RowMod  RowIdentIdx RowUserProp     �  �     RowObjUpd   p          x          �          �          �          �          �          �          �          �          �          �          �          NroItm  codmat  DesMat  DesMar  UndVta  CanDes  ImpLin  RowNum  RowIdent    RowMod  RowIdentIdx RowUserProp ChangedFields   !           !  
   appSrvUtils 4!        !     xiRocketIndexLimit  \!        H!  
   gshAstraAppserver   �!        p!  
   gshSessionManager   �!        �!  
   gshRIManager    �!        �!  
   gshSecurityManager  �!        �!  
   gshProfileManager   $"  	 	     "  
   gshRepositoryManager    P"  
 
     8"  
   gshTranslationManager   t"        d"  
   gshWebManager   �"        �"     gscSessionId    �"        �"     gsdSessionObj   �"        �"  
   gshFinManager   #        �"  
   gshGenManager   (#        #  
   gshAgnManager   L#        <#     gsdTempUniqueID l#        `#     gsdUserObj  �#        �#     gsdRenderTypeObj    �#        �#     gsdSessionScopeObj  �#       �#  
   ghProp  �#       �#  
   ghADMProps  $       $  
   ghADMPropsBuf   D$       0$     glADMLoadFromRepos  `$       X$     glADMOk �$       t$  
   ghContainer �$    	   �$     cObjectName �$    
   �$     iStart  �$       �$     cAppService �$       �$     cASDivision (%       %     cServerOperatingMode    L%       <%     cContainerType  p%       `%     cQueryString    �%       �%  
   hRowObject  �%       �%  
   hDataQuery  �%       �%     cColumns             �%     cDataFieldDefs  &    X  &  ttCcbDDocu  ,&    H   &  RowObject         X  <&  RowObjUpd          "   E   �   �   �   �   -  .  /  0  G  S  T  U  W  Y  Z  [  _  `  c  d  e  f  h  j  l  n  o  p  s  u  v  x  y  z  {  |  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  $	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	   
  P
  Q
  S
  T
  U
  V
  W
  X
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
  n
  o
  p
  q
  r
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
  �
  �
                	  
                                      �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  <  �  �  �  �               -  ?  ^  `  u  �      0  @  A  B  E  F  G  K  N  O  l  �  �  2  3  ?  c  �  �  �  �  �  P  3  4  5  6  ;  A  H  �  �  �  �  �  �       *  D  E  O  i  �  �  �  �  �  �      �  D:\newsie\on_in_co\aplic\ccb\dt-nota-cr-db-detail.w  t*  ��  C:\Progress\OpenEdge\src\adm2\data.i �*  �) . %C:\Progress\OpenEdge\src\adm2\custom\datacustom.i    �*  �� - C:\Progress\OpenEdge\src\adm2\robjflds.i $+  -� , D:\newsie\on_in_co\aplic\ccb\dt-nota-cr-db-detail.i  X+  �   C:\Progress\OpenEdge\src\adm2\query.i    �+  z + C:\Progress\OpenEdge\src\adm2\delrecst.i �+  `W * C:\Progress\OpenEdge\src\adm2\tblprep.i   ,   ) %C:\Progress\OpenEdge\src\adm2\custom\querycustom.i   4,  � ! C:\Progress\OpenEdge\src\adm2\dataquery.i    t,  �Z ( %C:\Progress\OpenEdge\src\adm2\custom\dataquerycustom.i   �,  �< " C:\Progress\OpenEdge\src\adm2\appserver.i    �,  �� ' %C:\Progress\OpenEdge\src\adm2\custom\appservercustom.i   (-  I� # C:\Progress\OpenEdge\src\adm2\smart.i    l-  Ds & C:\Progress\OpenEdge\gui\fn  �-  tw % %C:\Progress\OpenEdge\src\adm2\custom\smartcustom.i   �-  Q. $ C:\Progress\OpenEdge\gui\set .  �>  C:\Progress\OpenEdge\src\adm2\dataprop.i 0.  ��  %C:\Progress\OpenEdge\src\adm2\custom\datapropcustom.i    d.  ��  %C:\Progress\OpenEdge\src\adm2\custom\dataprtocustom.i    �.  �K  C:\Progress\OpenEdge\src\adm2\qryprop.i  �.  -�  %C:\Progress\OpenEdge\src\adm2\custom\qrypropcustom.i  /  ��  %C:\Progress\OpenEdge\src\adm2\custom\qryprtocustom.i `/   
 C:\Progress\OpenEdge\src\adm2\dataqueryprop.i    �/  �d  %C:\Progress\OpenEdge\src\adm2\custom\dataquerypropcustom.i   �/  ��  %C:\Progress\OpenEdge\src\adm2\custom\dataqueryprtocustom.i   $0  �l  C:\Progress\OpenEdge\src\adm2\appsprop.i l0  ɏ  %C:\Progress\OpenEdge\src\adm2\custom\appspropcustom.i    �0  V  %C:\Progress\OpenEdge\src\adm2\custom\appsprtocustom.i    �0  i$  C:\Progress\OpenEdge\src\adm2\smrtprop.i (1  �j  C:\Progress\OpenEdge\gui\get \1  �  %C:\Progress\OpenEdge\src\adm2\custom\smrtpropcustom.i    �1  ��  %C:\Progress\OpenEdge\src\adm2\custom\smrtprtocustom.i    �1  ��  C:\Progress\OpenEdge\src\adm2\smrtprto.i 2  Su  C:\Progress\OpenEdge\src\adm2\globals.i  @2  M�  %C:\Progress\OpenEdge\src\adm2\custom\globalscustom.i t2  )a  %C:\Progress\OpenEdge\src\adm2\custom\smartdefscustom.i   �2  �  C:\Progress\OpenEdge\src\adm2\appsprto.i �2  ��  %C:\Progress\OpenEdge\src\adm2\custom\appserverdefscustom.i   ,3  ��  C:\Progress\OpenEdge\src\adm2\dataqueryprto.i    t3  ª  %C:\Progress\OpenEdge\src\adm2\custom\dataquerydefscustom.i   �3  �� 	 C:\Progress\OpenEdge\src\adm2\qryprto.i  �3  �  %C:\Progress\OpenEdge\src\adm2\custom\querydefscustom.i   ,4  �`  C:\Progress\OpenEdge\src\adm2\dataprto.i p4  �  %C:\Progress\OpenEdge\src\adm2\custom\datadefscustom.i    �4  e�  !C:\Progress\OpenEdge\gui\adecomm\appserv.i   �4  �E    D:\newsie\on_in_co\aplic\ccb\dt-nota-cr-db-detail_cl.w          �      d5  �        t5  [  �     �5     �  &   �5  �   3     �5     �  .   �5  �   �     �5     �     �5  �   �     �5     �  $   �5  �   �     6     k  $   6  �   i     $6     G  $   46  �   D     D6     "  $   T6  �         d6     �  $   t6  �   �     �6     �  $   �6  �   �     �6     �  $   �6  �   �     �6     �  $   �6  �   �     �6     l  -   �6  �   h     7     `  ,   7  k   &     $7  �        47        +   D7  �  �      T7     �  +   d7  �  �      t7     �  +   �7  �  �      �7     �  +   �7  �  �      �7     �  +   �7  �  �      �7     o  +   �7  �  l      �7     R  +   8  �  O      8     5  +   $8  �  2      48       +   D8  �        T8     �  +   d8  �  �      t8     �  +   �8  �  �      �8     �  +   �8  �  �      �8     �  +   �8  �  �      �8     �  +   �8  �  �      �8     j  +   9  �  g      9     M  +   $9  �  J      49     0  +   D9  �  -      T9       +   d9  �  �      t9     �  $   �9  �  �      �9     �  $   �9  j  �      �9     g  $   �9  i  f      �9     D  $   �9  h  C      �9     !  $   :  ^        :     �  *   $:  ]  �      4:     �  *   D:  \  �      T:     �  *   d:  [  �      t:     |  *   �:  Z  {      �:     U  *   �:  Y  T      �:     .  *   �:  X  -      �:       *   �:  W        �:     �  *   ;  V  �      ;     �  *   $;  U  �      4;     �  *   D;  T  �      T;     k  *   d;  S  j      t;     D  *   �;  R  C      �;       *   �;  Q        �;     �  *   �;  P  �      �;     �  *   �;  O  �      �;     �  *   <  N  �      <     �  *   $<  M  �      4<     Z  *   D<  ?  L      T<     *  $   d<    �      t<     �  $   �<  �   L      �<     �  )   �<  g   �      �<  a   �  !   �<       (   �<  _   }  !   �<     [  $   �<  ]   Y  !   =     7  $   =  I   #  !   $=  �     "   4=     �  '   D=  �   �  "   T=     �  $   d=  �   �  "   t=     x  $   �=  �   v  "   �=     T  $   �=  g   :  "   �=          �=  O     "   �=  �   �  #   �=     �  &   �=  �   [  #   >       %   >  �   �  #   $>     �  $   4>  �   �  #   D>     �  $   T>  �   �  #   d>     �  $   t>  �   �  #   �>     m  $   �>  �   Y  #   �>     7  $   �>  }   +  #   �>     	  $   �>     �  #   �>     ?  "   �>     �  !   ?     �      ?     E     $?  �   <     4?  O   .     D?          T?     �     d?  �   �     t?  �   �     �?  O        �?     n     �?           �?  y   �
     �?  �   �
  
   �?  G   �
     �?     �
     �?     �
     @  c   (
  
   @  x    
     $@  M   
     4@     �	     D@     �	     T@  a   �	     d@  �  v	     t@     W	     �@  �  $	     �@  O   	     �@     	     �@     �     �@  �   �     �@     �     �@          �@  x        A     �     A     r     $A     n     4A     Z     DA     A     TA  Q   1     dA     �     tA     �     �A     �     �A     q     �A  ]   k  
   �A     a     �A       
   �A          �A     �  
   �A  Z   �     B       	   B     �     $B     �     4B     �     DB  c   u     TB     S     dB          tB     �      �B     �      �B     �      �B     &      �B           �B           