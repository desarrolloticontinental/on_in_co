	��V7�K�6   �                                              U& 36F000EFutf-8 MAIN O:\on_in_co\Util\dalmcmov.w,, PROCEDURE disable_UI,, PROCEDURE serverCommit,,INPUT-OUTPUT RowObjUpd TABLE,OUTPUT pcMessages CHARACTER,OUTPUT pcUndoIds CHARACTER PROCEDURE remoteCommit,,INPUT-OUTPUT pcContext CHARACTER,INPUT-OUTPUT RowObjUpd TABLE,OUTPUT pcMessages CHARACTER,OUTPUT pcUndoIds CHARACTER PROCEDURE pushTableAndValidate,,INPUT pcValType CHARACTER,INPUT-OUTPUT RowObjUpd TABLE PROCEDURE pushRowObjUpdTable,,INPUT RowObjUpd TABLE PROCEDURE initProps,, PROCEDURE start-super-proc,,INPUT pcProcName CHARACTER PROCEDURE adm-clone-props,, PROCEDURE viewObject,, PROCEDURE toggleData,,INPUT plEnabled LOGICAL PROCEDURE showMessageProcedure,,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL PROCEDURE returnFocus,,INPUT hTarget HANDLE PROCEDURE repositionObject,,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL PROCEDURE removeLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE removeAllLinks,, PROCEDURE modifyUserLinks,,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE PROCEDURE modifyListProperty,,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER PROCEDURE hideObject,, PROCEDURE exitObject,, PROCEDURE editInstanceProperties,, PROCEDURE displayLinks,, PROCEDURE createControls,, PROCEDURE changeCursor,,INPUT pcCursor CHARACTER PROCEDURE applyEntry,,INPUT pcField CHARACTER PROCEDURE adjustTabOrder,,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER PROCEDURE addMessage,,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER PROCEDURE addLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE unbindServer,,INPUT pcMode CHARACTER PROCEDURE runServerObject,,INPUT phAppService HANDLE PROCEDURE disconnectObject,, PROCEDURE destroyObject,, PROCEDURE bindServer,, PROCEDURE transferDBRow,,INPUT pcRowIdent CHARACTER,INPUT piRowNum INTEGER PROCEDURE startFilter,, PROCEDURE releaseDBRow,, PROCEDURE refetchDBRow,,INPUT phRowObjUpd HANDLE PROCEDURE filterContainerHandler,,INPUT phFilterContainer HANDLE PROCEDURE fetchDBRowForUpdate,, PROCEDURE confirmContinue,,INPUT-OUTPUT pioCancel LOGICAL PROCEDURE compareDBRow,, PROCEDURE bufferCopyDBToRO,,INPUT phRowObj HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER PROCEDURE assignDBRow,,INPUT phRowObjUpd HANDLE PROCEDURE updateState,,INPUT pcState CHARACTER PROCEDURE updateQueryPosition,, PROCEDURE updateAddQueryWhere,,INPUT pcWhere CHARACTER,INPUT pcField CHARACTER PROCEDURE undoTransaction,, PROCEDURE transferToExcel,,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT plUseExisting LOGICAL,INPUT piMaxRecords INTEGER PROCEDURE synchronizeProperties,,INPUT pcPropertiesForServer CHARACTER,OUTPUT pcPropertiesForClient CHARACTER PROCEDURE submitValidation,,INPUT pcValueList CHARACTER,INPUT pcUpdColumns CHARACTER PROCEDURE submitForeignKey,,INPUT pcRowIdent CHARACTER,INPUT-OUTPUT pcValueList CHARACTER,INPUT-OUTPUT pcUpdColumns CHARACTER PROCEDURE submitCommit,,INPUT pcRowIdent CHARACTER,INPUT plReopen LOGICAL PROCEDURE startServerObject,, PROCEDURE setPropertyList,,INPUT pcProperties CHARACTER PROCEDURE serverFetchRowObjUpdTable,,OUTPUT phRowObjUpd TABLE-HANDLE PROCEDURE serverSendRows,,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER,OUTPUT phRowObject TABLE-HANDLE PROCEDURE saveContextAndDestroy,,OUTPUT pcContext CHARACTER PROCEDURE rowObjectState,,INPUT pcState CHARACTER PROCEDURE retrieveFilter,, PROCEDURE restartServerObject,, PROCEDURE remoteSendRows,,INPUT-OUTPUT piocContext CHARACTER,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT pioRowsReturned INTEGER,OUTPUT phRowObject TABLE-HANDLE,OUTPUT pocMessages CHARACTER PROCEDURE refreshRow,, PROCEDURE printToCrystal,,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT piMaxRecords INTEGER PROCEDURE isUpdatePending,,INPUT-OUTPUT plUpdate LOGICAL PROCEDURE initializeServerObject,, PROCEDURE initializeObject,, PROCEDURE home,, PROCEDURE genContextList,,OUTPUT pcContext CHARACTER PROCEDURE fetchPrev,, PROCEDURE fetchNext,, PROCEDURE fetchLast,, PROCEDURE fetchFirst,, PROCEDURE fetchBatch,,INPUT plForwards LOGICAL PROCEDURE endClientDataRequest,, PROCEDURE destroyServerObject,, PROCEDURE describeSchema,,INPUT pcSdoName CHARACTER,OUTPUT hTtSchema TABLE-HANDLE PROCEDURE dataAvailable,,INPUT pcRelative CHARACTER PROCEDURE copyColumns,,INPUT pcViewColList CHARACTER,INPUT phDataQuery HANDLE PROCEDURE commitTransaction,, PROCEDURE clientSendRows,,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER PROCEDURE batchServices,,INPUT pcServices CHARACTER,OUTPUT pcValues CHARACTER FUNCTION deleteRecordStatic,logical,INPUT piTableIndex INTEGER FUNCTION getRowObjUpdStatic,widget-handle, FUNCTION Signature,CHARACTER,INPUT pcName CHARACTER FUNCTION showmessage,LOGICAL,INPUT pcMessage CHARACTER FUNCTION setUserProperty,LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION setUIBMode,LOGICAL,INPUT pcMode CHARACTER FUNCTION setTranslatableProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setSupportedLinks,LOGICAL,INPUT pcLinkList CHARACTER FUNCTION setRunAttribute,LOGICAL,INPUT cRunAttribute CHARACTER FUNCTION setPhysicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setPhysicalObjectName,LOGICAL,INPUT cTemp CHARACTER FUNCTION setPassThroughLinks,LOGICAL,INPUT pcLinks CHARACTER FUNCTION setParentDataKey,LOGICAL,INPUT cParentDataKey CHARACTER FUNCTION setObjectVersion,LOGICAL,INPUT cObjectVersion CHARACTER FUNCTION setObjectParent,LOGICAL,INPUT phParent HANDLE FUNCTION setObjectName,LOGICAL,INPUT pcName CHARACTER FUNCTION setLogicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setLogicalObjectName,LOGICAL,INPUT c CHARACTER FUNCTION setInstanceProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setDynamicObject,LOGICAL,INPUT lTemp LOGICAL FUNCTION setDesignDataObject,LOGICAL,INPUT pcDataObject CHARACTER FUNCTION setDBAware,LOGICAL,INPUT lAware LOGICAL FUNCTION setDataTargetEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setDataTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setDataSourceNames,LOGICAL,INPUT pcSourceNames CHARACTER FUNCTION setDataSourceEvents,LOGICAL,INPUT pcEventsList CHARACTER FUNCTION setDataSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDataLinksEnabled,LOGICAL,INPUT lDataLinksEnabled LOGICAL FUNCTION setContainerSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setContainerSource,LOGICAL,INPUT phObject HANDLE FUNCTION setContainerHidden,LOGICAL,INPUT plHidden LOGICAL FUNCTION setChildDataKey,LOGICAL,INPUT cChildDataKey CHARACTER FUNCTION reviewMessages,CHARACTER, FUNCTION propertyType,CHARACTER,INPUT pcPropName CHARACTER FUNCTION messageNumber,CHARACTER,INPUT piMessage INTEGER FUNCTION mappedEntry,CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER FUNCTION linkProperty,CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER FUNCTION linkHandles,CHARACTER,INPUT pcLink CHARACTER FUNCTION instancePropertyList,CHARACTER,INPUT pcPropList CHARACTER FUNCTION getUserProperty,CHARACTER,INPUT pcPropName CHARACTER FUNCTION getUIBMode,CHARACTER, FUNCTION getTranslatableProperties,CHARACTER, FUNCTION getSupportedLinks,CHARACTER, FUNCTION getRunAttribute,CHARACTER, FUNCTION getQueryObject,LOGICAL, FUNCTION getPropertyDialog,CHARACTER, FUNCTION getPhysicalVersion,CHARACTER, FUNCTION getPhysicalObjectName,CHARACTER, FUNCTION getPassThroughLinks,CHARACTER, FUNCTION getParentDataKey,CHARACTER, FUNCTION getObjectVersionNumber,CHARACTER, FUNCTION getObjectVersion,CHARACTER, FUNCTION getObjectParent,HANDLE, FUNCTION getObjectPage,INTEGER, FUNCTION getObjectName,CHARACTER, FUNCTION getObjectInitialized,LOGICAL, FUNCTION getObjectHidden,LOGICAL, FUNCTION getLogicalVersion,CHARACTER, FUNCTION getLogicalObjectName,CHARACTER, FUNCTION getInstanceProperties,CHARACTER, FUNCTION getDynamicObject,LOGICAL, FUNCTION getDesignDataObject,CHARACTER, FUNCTION getDBAware,LOGICAL, FUNCTION getDataTargetEvents,CHARACTER, FUNCTION getDataTarget,CHARACTER, FUNCTION getDataSourceNames,CHARACTER, FUNCTION getDataSourceEvents,CHARACTER, FUNCTION getDataSource,HANDLE, FUNCTION getDataLinksEnabled,LOGICAL, FUNCTION getContainerType,CHARACTER, FUNCTION getContainerSourceEvents,CHARACTER, FUNCTION getContainerSource,HANDLE, FUNCTION getContainerHidden,LOGICAL, FUNCTION getContainerHandle,HANDLE, FUNCTION getChildDataKey,CHARACTER, FUNCTION fetchMessages,CHARACTER, FUNCTION assignLinkProperty,LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION anyMessage,LOGICAL, FUNCTION setServerOperatingMode,LOGICAL,INPUT pcServerOperatingMode CHARACTER FUNCTION setServerFileName,LOGICAL,INPUT pcFileName CHARACTER FUNCTION setASUsePrompt,LOGICAL,INPUT plFlag LOGICAL FUNCTION setASInitializeOnRun,LOGICAL,INPUT plInitialize LOGICAL FUNCTION setASInfo,LOGICAL,INPUT pcInfo CHARACTER FUNCTION setASHandle,LOGICAL,INPUT phASHandle HANDLE FUNCTION setASDivision,LOGICAL,INPUT pcDivision CHARACTER FUNCTION setAppService,LOGICAL,INPUT pcAppService CHARACTER FUNCTION runServerProcedure,HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE FUNCTION getServerOperatingMode,CHARACTER, FUNCTION getServerFileName,CHARACTER, FUNCTION getASUsePrompt,LOGICAL, FUNCTION getASInitializeOnRun,LOGICAL, FUNCTION getASInfo,CHARACTER, FUNCTION getASHasStarted,LOGICAL, FUNCTION getASHandle,HANDLE, FUNCTION getAsDivision,CHARACTER, FUNCTION getASBound,LOGICAL, FUNCTION getAppService,CHARACTER, FUNCTION whereClauseBuffer,CHARACTER,INPUT pcWhere CHARACTER FUNCTION setQueryWhere,LOGICAL,INPUT pcWhere CHARACTER FUNCTION setQueryString,LOGICAL,INPUT pcQueryString CHARACTER FUNCTION setQuerySort,LOGICAL,INPUT pcSort CHARACTER FUNCTION setQueryPosition,LOGICAL,INPUT pcPosition CHARACTER FUNCTION rowidWhereCols,CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER FUNCTION rowidWhere,CHARACTER,INPUT pcWhere CHARACTER FUNCTION resolveBuffer,CHARACTER,INPUT pcBuffer CHARACTER FUNCTION removeQuerySelection,LOGICAL,INPUT pcColumns CHARACTER,INPUT pcOperators CHARACTER FUNCTION removeForeignKey,LOGICAL, FUNCTION refreshRowident,CHARACTER,INPUT pcRowident CHARACTER FUNCTION newWhereClause,CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcExpression CHARACTER,INPUT pcWhere CHARACTER,INPUT pcAndOr CHARACTER FUNCTION newQueryWhere,CHARACTER,INPUT pcWhere CHARACTER FUNCTION newQueryValidate,CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcExpression CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER FUNCTION newQueryString,CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcAndOr CHARACTER FUNCTION insertExpression,CHARACTER,INPUT pcWhere CHARACTER,INPUT pcExpression CHARACTER,INPUT pcAndOr CHARACTER FUNCTION indexInformation,CHARACTER,INPUT pcQuery CHARACTER,INPUT plUseTableSep LOGICAL,INPUT pcIndexInfo CHARACTER FUNCTION getTargetProcedure,HANDLE, FUNCTION getQueryWhere,CHARACTER, FUNCTION getQueryString,CHARACTER, FUNCTION getQuerySort,CHARACTER, FUNCTION getQueryPosition,CHARACTER, FUNCTION getForeignValues,CHARACTER, FUNCTION getDataColumns,CHARACTER, FUNCTION excludeColumns,CHARACTER,INPUT iTable INTEGER FUNCTION dbColumnHandle,HANDLE,INPUT pcColumn CHARACTER FUNCTION dbColumnDataName,CHARACTER,INPUT pcDbColumn CHARACTER FUNCTION columnValMsg,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnTable,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnQuerySelection,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnDbColumn,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnDataType,CHARACTER,INPUT pcColumn CHARACTER FUNCTION bufferWhereClause,CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcWhere CHARACTER FUNCTION bufferCompareDBToRO,LOGICAL,INPUT phRowObjUpd HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER FUNCTION assignQuerySelection,LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER FUNCTION addQueryWhere,LOGICAL,INPUT pcWhere CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER FUNCTION getObjectType,character, FUNCTION updateRow,LOGICAL,INPUT pcKeyValues CHARACTER,INPUT pcValueList CHARACTER FUNCTION submitRow,LOGICAL,INPUT pcRowIdent CHARACTER,INPUT pcValueList CHARACTER FUNCTION rowValues,CHARACTER,INPUT pcColumns CHARACTER,INPUT pcFormat CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION rowAvailable,LOGICAL,INPUT pcDirection CHARACTER FUNCTION prepareQuery,LOGICAL,INPUT pcQuery CHARACTER FUNCTION openQuery,LOGICAL, FUNCTION openDataQuery,LOGICAL,INPUT pcPosition CHARACTER FUNCTION hasForeignKeyChanged,LOGICAL, FUNCTION getLastCommitErrorType,CHARACTER, FUNCTION firstRowIds,CHARACTER,INPUT pcQueryString CHARACTER FUNCTION findRowWhere,LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER FUNCTION findRow,LOGICAL,INPUT pcKeyValues CHARACTER FUNCTION fetchRowIdent,CHARACTER,INPUT pcRowIdent CHARACTER,INPUT pcViewColList CHARACTER FUNCTION fetchRow,CHARACTER,INPUT piRow INTEGER,INPUT pcViewColList CHARACTER FUNCTION deleteRow,LOGICAL,INPUT pcRowIdent CHARACTER FUNCTION createRow,LOGICAL,INPUT pcValueList CHARACTER FUNCTION copyRow,CHARACTER,INPUT pcViewColList CHARACTER FUNCTION colValues,CHARACTER,INPUT pcViewColList CHARACTER FUNCTION columnProps,CHARACTER,INPUT pcColList CHARACTER,INPUT pcPropList CHARACTER FUNCTION closeQuery,LOGICAL, FUNCTION canNavigate,LOGICAL, FUNCTION cancelRow,CHARACTER, FUNCTION addRow,CHARACTER,INPUT pcViewColList CHARACTER TEMP-TABLE RowObjUpd 0,RowNum:RowNum 0 NO,TipMov character 0 0,CodMov integer 1 0,NroSer integer 2 0,NroDoc integer 3 0,FchDoc date 4 0,RowNum integer 5 0,RowIdent character 6 0,RowMod character 7 0,RowIdentIdx character 8 0,RowUserProp character 9 0,ChangedFields character 10 0       �              �             �� �  L�              4�              �>     +   � �  W   �� `  X   �   Y    �   [   0�   \   H� <  ]   ��    ^   �� 0  `   ? �� �  iSO8859-1                                                                           �    �                                      �                   �                  �    �   ��   T�              ��  �   T      `          �                                             PROGRESS                         H           
    
                    �              �                                                                                                     
               INTEGRAL                         PROGRESS                         x        �                                �)�K               *e                              �  l                      h  |  � (     CODCIACODALMTIPMOVCODMOVNRODOCFCHDOCNRORF1NRORF2CODPROCODCLICODVENOBSERVTOTITMCODMONTPOCMBFLGESTALMDESUSUARIONROSERFLGSITHORSALHORRCPFCHANUCODDOCCODREFNROREFCODTRAFLGCBDFCHCBDFLGFACNROFACNOMREFIMPMN1IMPMN2CCOAREAIMPIGVMODADQNRORF3HRADOC                                                                      	          
                                                                                                                                                                                                                                       !          "          #          $          %          &          '          (          )          �  �      p  
    
                  \  $             �                                                                                          �          
  �  �        
    
                    �             �                                                                                          �          
  L        �  
    
                  �  |             8                                                                                                    
  �        t  
    
                  `  (             �                                                                                                    
  �  .         
    
                    �             �                                                                                          .          
  P	  @      �  
    
                  �  �	  	           <	                                                                                          @          
  �	  U      x	  
    
                  d	  ,
  
           �	                                                                                          U          
  �
  k      $
  
    
                  
  �
             �
                                                                                          k          
  T  y      �
                         �
  �             @                                                                                          y               �      |                        h  0             �                                                                                          �            �  �      (  
    
                    �             �                                                                                          �          
  X  �      �  
    
                  �  �             D                                                                                          �          
    �      �  
    
                  l  4             �                                                                                          �          
  �  �      ,                          �             �                                                                                          �            \  �      �                        �  �             H                                                                                          �              �      �                        p  8             �                                                                                          �                �      0                          �             �                                                                                          �                     �       {  X  `     |  {  �      �  
       {             �          �      �              �       �  X  (     D  �  �!      �         �         �               �      �                 h�                                               l�            X  L l�                �         
             
             
                                         
                                                                                                               
             
                                          L   \   l   |   �   �   �   �   �   �   �   �       ,  <  L  \      L   \   l   |   �   �   �   �   �   �   �   �      ,  <  L  \                                                                                                                                     	                  
                                                   d  l  p  �  �          �             �  �  �  �  �          �             �       $                           (  0  8  \  H          `             t  |  �  �  �          �             �  �  �  �                             �  �  �  �                              �                                     $  ,  8                             <  H  P  \                                                                          TipMov  X   Tipo de movimiento  Tp.!movmto.     Tipo de movimiento  CodMov  99  C�digo de movimiento    C�digo!movimto. 0   C�digo de movimiento    NroSer  999 Numero Serie    Numero!Serie    0   NroDoc  999999  No. documento   Numero de!documento 0   N�mero de documento FchDoc  99/99/9999  Fecha   Fecha!docum TODAY   Fecha de documento  RowNum  ->,>>>,>>9  RowNum  0   RowIdent    x(8)    RowIdent        RowMod  x(8)    RowMod      RowIdentIdx x(8)    RowIdentIdx     RowUserProp x(8)    RowUserProp     �  ���������    �          �        �        �                �     i     i     i     		 	
 	    g  n  u  |  �  �  �  �  �  �                                                                                                                                     	                  
                                                                            ,             0             D  L  P  x  h          |             �  �  �  �  �                         �  �  �  �  �          �                 $  8  ,          @             T  \  h  p                             t  �  �  �                              �  �  �  �                             �  �  �  �                             �  �  �  �                              �      $                                                                          TipMov  X   Tipo de movimiento  Tp.!movmto.     Tipo de movimiento  CodMov  99  C�digo de movimiento    C�digo!movimto. 0   C�digo de movimiento    NroSer  999 Numero Serie    Numero!Serie    0   NroDoc  999999  No. documento   Numero de!documento 0   N�mero de documento FchDoc  99/99/9999  Fecha   Fecha!docum TODAY   Fecha de documento  RowNum  ->,>>>,>>9  RowNum  0   RowIdent    x(8)    RowIdent        RowMod  x(8)    RowMod      RowIdentIdx x(8)    RowIdentIdx     RowUserProp x(8)    RowUserProp     ChangedFields   x(8)    ChangedFields       �  ���������    �          �        �        �                �     i     i     i     		 	
 	    g  n  u  |  �  �  �  �  �  �  �    ��                            ����                            �    ��                    �O    �   ��                    �    undefined                                                               �       ��  �   l   ��  ��                    �����               D�                    O   ����    e�          O   ����    R�          O   ����    ��      t       �   �              4   ����     /                                    3   ����       $     H  ���                       8      
                       � ߱        �  �      D       �     9          ��    �   �  4      d       4   ����d                 D                      ��                  �   �                   ���                       �   �  �  	  �   x                                        3   ����|       O   �   ��  ��  �   batchServices                               4        ��                  N  Q  L              �(                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             d               ��                  �           ��                            ����                            clientSendRows                              �  p      ��                  S  Y  �              |*                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             �               ��                �               ��   <                            ��   d             0               ��                  X           ��                            ����                            commitTransaction                               X  @      ��                  [  \  p              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            copyColumns                             X  @      ��                  ^  a  p              <�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             �               �� 
                 �  
         ��                            ����                            dataAvailable                               �  �      ��                  c  e  �              �5X	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            describeSchema                              �	  �	      ��                  g  j  �	              ��W	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   <
             
               �� 
          �       0
  
         ��                            ����                            destroyServerObject                             0        ��                  l  m  H              \X	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            endClientDataRequest                                <  $      ��                  o  p  T              ��X	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchBatch                              <  $      ��                  r  t  T              ��X	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  l           ��                            ����                            fetchFirst                              d  L      ��                  v  w  |              ��W	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchLast                               d  L      ��                  y  z  |              p(Y	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchNext                               d  L      ��                  |  }  |              �(Y	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchPrev                               d  L      ��                    �  |              �)Y	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            genContextList                              h  P      ��                  �  �  �              yY	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            home                                �  t      ��                  �  �  �              ��X	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �  |      ��                  �  �  �              D�X	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              �  �      ��                  �  �  �               �W	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            isUpdatePending                             �  �      ��                  �  �  �              ��W	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            printToCrystal                              �  �      ��                  �  �  �              `X	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   4                             ��   \             (               ��                  P           ��                            ����                            refreshRow                              H  0      ��                  �  �  `               ~Y	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            remoteSendRows                              L  4      ��                  �  �  d              h�X	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             |               ��   �             �               ��                 �               ��   (             �               ��   P                            ��   x             D               �� 
  �      �       l  
             ��                  �           ��                            ����                            restartServerObject                             �  |      ��                  �  �  �              dX	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            retrieveFilter                              �  �      ��                  �  �  �              @�Y	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            rowObjectState                              �  �      ��                  �  �  �              ��Y	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            saveContextAndDestroy                               �  �      ��                  �  �  �              XY	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                               ��                            ����                            serverSendRows                              �   �       ��                  �  �  !              dX	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   `!             ,!               ��   �!             T!               ��   �!             |!               ��   �!             �!               ��    "             �!               �� 
          �       �!  
         ��                            ����                            serverFetchRowObjUpdTable                               �"  �"      ��                  �  �  #              $�W	                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
          �       ,#  
         ��                            ����                            setPropertyList                             ($  $      ��                  �  �  @$              ��M	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  X$           ��                            ����                            serverSendRows                              T%  <%      ��                  �  �  l%              LoO	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �%             �%               ��   �%             �%               ��   &             �%               ��   0&             �%               ��   X&             $&               �� 
          �       L&  
         ��                            ����                            startServerObject                               L'  4'      ��                  �  �  d'              `�P	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            submitCommit                                P(  8(      ��                  �  �  h(              �O	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �(             �(               ��                  �(           ��                            ����                            submitForeignKey                                �)  �)      ��                  �  �  �)              �0P	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   *             �)               ��   4*              *               ��                  (*           ��                            ����                            submitValidation                                (+  +      ��                  �  �  @+              ��O	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �+             X+               ��                  �+           ��                            ����                            synchronizeProperties                               �,  l,      ��                  �  �  �,              x$O	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �,             �,               ��                  �,           ��                            ����                            transferToExcel                             �-  �-      ��                       �-              ,�O	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   <.             .               ��   d.             0.               ��   �.             X.               ��                  �.           ��                            ����                            undoTransaction                             |/  d/      ��                      �/              xd�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            updateAddQueryWhere                             �0  l0      ��                  
    �0              Ty�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �0             �0               ��                  �0           ��                            ����                            updateQueryPosition                             �1  �1      ��                      �1              ,e�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            updateState                             �2  �2      ��                      �2              �e�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  3           ��                            ����                            addRow          t3      �3     S       CHARACTER,INPUT pcViewColList CHARACTER cancelRow   |3      �3      �3   	 Z       CHARACTER,  canNavigate �3      �3      (4    d       LOGICAL,    closeQuery  4      44      `4   
 p       LOGICAL,    columnProps @4      l4      �4    {       CHARACTER,INPUT pcColList CHARACTER,INPUT pcPropList CHARACTER  colValues   x4      �4      5   	 �       CHARACTER,INPUT pcViewColList CHARACTER copyRow �4      ,5      T5    �       CHARACTER,INPUT pcViewColList CHARACTER createRow   45      |5      �5   	 �       LOGICAL,INPUT pcValueList CHARACTER deleteRow   �5      �5      �5   	 �       LOGICAL,INPUT pcRowIdent CHARACTER  fetchRow    �5      6      H6  	  �       CHARACTER,INPUT piRow INTEGER,INPUT pcViewColList CHARACTER fetchRowIdent   (6      �6      �6  
  �       CHARACTER,INPUT pcRowIdent CHARACTER,INPUT pcViewColList CHARACTER  findRow �6      �6       7    �       LOGICAL,INPUT pcKeyValues CHARACTER findRowWhere     7      D7      t7    �       LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER  firstRowIds T7      �7      �7    �       CHARACTER,INPUT pcQueryString CHARACTER getLastCommitErrorType  �7       8      X8    �       CHARACTER,  hasForeignKeyChanged    88      d8      �8    �       LOGICAL,    openDataQuery   |8      �8      �8          LOGICAL,INPUT pcPosition CHARACTER  openQuery   �8      �8      (9   	       LOGICAL,    prepareQuery    9      49      d9    )      LOGICAL,INPUT pcQuery CHARACTER rowAvailable    D9      �9      �9    6      LOGICAL,INPUT pcDirection CHARACTER rowValues   �9      �9      :   	 C      CHARACTER,INPUT pcColumns CHARACTER,INPUT pcFormat CHARACTER,INPUT pcDelimiter CHARACTER    submitRow   �9      `:      �:   	 M      LOGICAL,INPUT pcRowIdent CHARACTER,INPUT pcValueList CHARACTER  updateRow   l:      �:      �:   	 W      LOGICAL,INPUT pcKeyValues CHARACTER,INPUT pcValueList CHARACTER getObjectType   �:      8;      h;    a      CHARACTER,  assignDBRow                             <  �;      ��                  �  �  <              x#�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 4<  
         ��                            ����                            bufferCopyDBToRO                                4=  =      ��                  �    L=              �=�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �=             d=  
             �� 
  �=             �=  
             ��   �=             �=               ��                  �=           ��                            ����                            compareDBRow                                �>  �>      ��                      �>              �&�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            confirmContinue                             �?  �?      ��                    
  �?              �)�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  @           ��                            ����                            dataAvailable                               A  �@      ��                       A              ���                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  8A           ��                            ����                            fetchDBRowForUpdate                             8B   B      ��                      PB              H'�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchFirst                              8C   C      ��                      PC              �'�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchLast                               8D   D      ��                      PD              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchNext                               8E   E      ��                      PE              ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchPrev                               8F   F      ��                      PF              |��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            filterContainerHandler                              DG  ,G      ��                    !  \G              t��                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 tG  
         ��                            ����                            initializeObject                                tH  \H      ��                  #  $  �H              x��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            refetchDBRow                                xI  `I      ��                  &  (  �I              X��                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �I  
         ��                            ����                            releaseDBRow                                �J  �J      ��                  *  +  �J              ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            startFilter                             �K  �K      ��                  -  .  �K              ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            transferDBRow                               �L  �L      ��                  0  3  �L              T��                    O   ����    e�          O   ����    R�          O   ����    ��            ��   M             �L               ��                   M           ��                            ����                            addQueryWhere   H;      hM      �M    �      LOGICAL,INPUT pcWhere CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER    assignQuerySelection    xM      �M      $N    �      LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER  bufferCompareDBToRO N      |N      �N    �      LOGICAL,INPUT phRowObjUpd HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER bufferWhereClause   �N      O      PO    �      CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcWhere CHARACTER  columnDataType  0O      �O      �O    �      CHARACTER,INPUT pcColumn CHARACTER  columnDbColumn  �O      �O      P    �      CHARACTER,INPUT pcColumn CHARACTER  columnQuerySelection    �O      4P      lP    �      CHARACTER,INPUT pcColumn CHARACTER  columnTable LP      �P      �P    �      CHARACTER,INPUT pcColumn CHARACTER  columnValMsg    �P      �P      Q     
      CHARACTER,INPUT pcColumn CHARACTER  dbColumnDataName    �P      4Q      hQ  !        CHARACTER,INPUT pcDbColumn CHARACTER    dbColumnHandle  HQ      �Q      �Q  "  (      HANDLE,INPUT pcColumn CHARACTER excludeColumns  �Q      �Q      R  #  7      CHARACTER,INPUT iTable INTEGER  getDataColumns  �Q      0R      `R  $  F      CHARACTER,  getForeignValues    @R      lR      �R  %  U      CHARACTER,  getQueryPosition    �R      �R      �R  &  f      CHARACTER,  getQuerySort    �R      �R      S  '  w      CHARACTER,  getQueryString  �R      (S      XS  (  �      CHARACTER,  getQueryWhere   8S      dS      �S  )  �      CHARACTER,  getTargetProcedure  tS      �S      �S  *  �      HANDLE, indexInformation    �S      �S      T  +  �      CHARACTER,INPUT pcQuery CHARACTER,INPUT plUseTableSep LOGICAL,INPUT pcIndexInfo CHARACTER   insertExpression    �S      lT      �T  ,  �      CHARACTER,INPUT pcWhere CHARACTER,INPUT pcExpression CHARACTER,INPUT pcAndOr CHARACTER  newQueryString  �T      �T      (U  -  �      CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcAndOr CHARACTER  newQueryValidate    U      �U      �U  .  �      CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcExpression CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER   newQueryWhere   �U      dV      �V  /  �      CHARACTER,INPUT pcWhere CHARACTER   newWhereClause  tV      �V      �V  0        CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcExpression CHARACTER,INPUT pcWhere CHARACTER,INPUT pcAndOr CHARACTER refreshRowident �V      XW      �W  1        CHARACTER,INPUT pcRowident CHARACTER    removeForeignKey    hW      �W      �W  2  #      LOGICAL,    removeQuerySelection    �W      �W      (X  3  4      LOGICAL,INPUT pcColumns CHARACTER,INPUT pcOperators CHARACTER   resolveBuffer   X      hX      �X  4  I      CHARACTER,INPUT pcBuffer CHARACTER  rowidWhere  xX      �X      �X  5 
 W      CHARACTER,INPUT pcWhere CHARACTER   rowidWhereCols  �X      Y      <Y  6  b      CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER    setQueryPosition    Y      �Y      �Y  7  q      LOGICAL,INPUT pcPosition CHARACTER  setQuerySort    �Y      �Y       Z  8  �      LOGICAL,INPUT pcSort CHARACTER  setQueryString   Z      @Z      pZ  9  �      LOGICAL,INPUT pcQueryString CHARACTER   setQueryWhere   PZ      �Z      �Z  :  �      LOGICAL,INPUT pcWhere CHARACTER whereClauseBuffer   �Z      �Z      [  ;  �      CHARACTER,INPUT pcWhere CHARACTER   bindServer                              �[  �[      ��                  �  �  �[              ��d	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �\  �\      ��                  �  �  �\              x�d	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyServerObject                             �]  �]      ��                  �  �  �]               �d	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disconnectObject                                �^  �^      ��                  �  �  �^              t�d	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              �_  �_      ��                  �  �  `              $�d	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            restartServerObject                             �`  �`      ��                  �  �  a              ��d	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            runServerObject                             �a  �a      ��                  �  �  b              ��d	                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 ,b  
         ��                            ����                            startServerObject                               ,c  c      ��                  �  �  Dc              h�d	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            unbindServer                                0d  d      ��                  �  �  Hd              h�d	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  `d           ��                            ����                            getAppService   �Z      �d      �d  <  �      CHARACTER,  getASBound  �d      e      0e  = 
 �      LOGICAL,    getAsDivision   e      <e      le  >  �      CHARACTER,  getASHandle Le      xe      �e  ?  �      HANDLE, getASHasStarted �e      �e      �e  @  �      LOGICAL,    getASInfo   �e      �e      f  A 	       CHARACTER,  getASInitializeOnRun    �e       f      Xf  B        LOGICAL,    getASUsePrompt  8f      df      �f  C         LOGICAL,    getServerFileName   tf      �f      �f  D  /      CHARACTER,  getServerOperatingMode  �f      �f      g  E  A      CHARACTER,  runServerProcedure  �f      $g      Xg  F  X      HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE   setAppService   8g      �g      �g  G  k      LOGICAL,INPUT pcAppService CHARACTER    setASDivision   �g      �g      $h  H  y      LOGICAL,INPUT pcDivision CHARACTER  setASHandle h      Hh      th  I  �      LOGICAL,INPUT phASHandle HANDLE setASInfo   Th      �h      �h  J 	 �      LOGICAL,INPUT pcInfo CHARACTER  setASInitializeOnRun    �h      �h      i  K  �      LOGICAL,INPUT plInitialize LOGICAL  setASUsePrompt  �h      <i      li  L  �      LOGICAL,INPUT plFlag LOGICAL    setServerFileName   Li      �i      �i  M  �      LOGICAL,INPUT pcFileName CHARACTER  setServerOperatingMode  �i      �i      j  N  �      LOGICAL,INPUT pcServerOperatingMode CHARACTER   addLink                             �j  �j      ��                  �  �  �j              < e	                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  <k             k  
             ��   dk             0k               �� 
                 Xk  
         ��                            ����                            addMessage                              Pl  8l      ��                  �  �  hl              �.e	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �l             �l               ��   �l             �l               ��                  �l           ��                            ����                            adjustTabOrder                              �m  �m      ��                  �  �  �m              (6e	                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  0n             �m  
             �� 
  Xn             $n  
             ��                  Ln           ��                            ����                            applyEntry                              Do  ,o      ��                  �  �  \o              �=e	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  to           ��                            ����                            changeCursor                                pp  Xp      ��                  �  �  �p              ,Be	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �p           ��                            ����                            createControls                              �q  �q      ��                  �  �  �q              �Fe	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �r  �r      ��                  �  �  �r              @Ge	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            displayLinks                                �s  �s      ��                  �  �  �s              �Je	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            editInstanceProperties                              �t  �t      ��                  �  �  �t              8Ke	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            exitObject                              �u  �u      ��                  �  �  �u              0Ne	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hideObject                              �v  �v      ��                  �  �  �v              �Ne	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �w  �w      ��                  �  �  �w              |Oe	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            modifyListProperty                              �x  �x      ��                  �  �  �x              �Re	                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  $y             �x  
             ��   Ly             y               ��   ty             @y               ��                  hy           ��                            ����                            modifyUserLinks                             dz  Lz      ��                  �  �  |z              �We	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �z             �z               ��   �z             �z               �� 
                 �z  
         ��                            ����                            removeAllLinks                              �{  �{      ��                  �  �  �{              �ge	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            removeLink                              �|  �|      ��                  �  �  �|              �ke	                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  D}             }  
             ��   l}             8}               �� 
                 `}  
         ��                            ����                            repositionObject                                `~  H~      ��                  �  �  x~              �ve	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �~             �~               ��                  �~           ��                            ����                            returnFocus                             �  �      ��                  �  �  �              �|e	                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �  
         ��                            ����                            showMessageProcedure                                �  ̀      ��                  �  �  ��              Ȁe	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   H�             �               ��                  <�           ��                            ����                            toggleData                              4�  �      ��                  �     L�              ��e	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  d�           ��                            ����                            viewObject                              \�  D�      ��                      t�              $�e	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            anyMessage  �i      ̃      ��  O 
 8      LOGICAL,    assignLinkProperty  ؃      �      8�  P  C      LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER   fetchMessages   �      ��      ��  Q  V      CHARACTER,  getChildDataKey ��      ̄      ��  R  d      CHARACTER,  getContainerHandle  ܄      �      <�  S  t      HANDLE, getContainerHidden  �      D�      x�  T  �      LOGICAL,    getContainerSource  X�      ��      ��  U  �      HANDLE, getContainerSourceEvents    ��      ��      ��  V  �      CHARACTER,  getContainerType    ܅      �      <�  W  �      CHARACTER,  getDataLinksEnabled �      H�      |�  X  �      LOGICAL,    getDataSource   \�      ��      ��  Y  �      HANDLE, getDataSourceEvents ��      ��      �  Z  �      CHARACTER,  getDataSourceNames  Ԇ       �      4�  [        CHARACTER,  getDataTarget   �      @�      p�  \         CHARACTER,  getDataTargetEvents P�      |�      ��  ]  .      CHARACTER,  getDBAware  ��      ��      �  ^ 
 B      LOGICAL,    getDesignDataObject ȇ      �      (�  _  M      CHARACTER,  getDynamicObject    �      4�      h�  `  a      LOGICAL,    getInstanceProperties   H�      t�      ��  a  r      CHARACTER,  getLogicalObjectName    ��      ��      ��  b  �      CHARACTER,  getLogicalVersion   Ј      ��      0�  c  �      CHARACTER,  getObjectHidden �      <�      l�  d  �      LOGICAL,    getObjectInitialized    L�      x�      ��  e  �      LOGICAL,    getObjectName   ��      ��      �  f  �      CHARACTER,  getObjectPage   ̉      ��      (�  g  �      INTEGER,    getObjectParent �      4�      d�  h  �      HANDLE, getObjectVersion    D�      l�      ��  i         CHARACTER,  getObjectVersionNumber  ��      ��      �  j        CHARACTER,  getParentDataKey    Ċ      ��      $�  k  (      CHARACTER,  getPassThroughLinks �      0�      d�  l  9      CHARACTER,  getPhysicalObjectName   D�      p�      ��  m  M      CHARACTER,  getPhysicalVersion  ��      ��      �  n  c      CHARACTER,  getPropertyDialog   ȋ      �      (�  o  v      CHARACTER,  getQueryObject  �      4�      d�  p  �      LOGICAL,    getRunAttribute D�      p�      ��  q  �      CHARACTER,  getSupportedLinks   ��      ��      ��  r  �      CHARACTER,  getTranslatableProperties   ��      �      (�  s  �      CHARACTER,  getUIBMode  �      4�      `�  t 
 �      CHARACTER,  getUserProperty @�      l�      ��  u  �      CHARACTER,INPUT pcPropName CHARACTER    instancePropertyList    |�      č      ��  v  �      CHARACTER,INPUT pcPropList CHARACTER    linkHandles ܍      $�      P�  w  	      CHARACTER,INPUT pcLink CHARACTER    linkProperty    0�      t�      ��  x  	      CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER mappedEntry ��      ��      �  y  	      CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER  messageNumber   �      x�      ��  z  (	      CHARACTER,INPUT piMessage INTEGER   propertyType    ��      ̏      ��  {  6	      CHARACTER,INPUT pcPropName CHARACTER    reviewMessages  ܏      $�      T�  |  C	      CHARACTER,  setChildDataKey 4�      `�      ��  }  R	      LOGICAL,INPUT cChildDataKey CHARACTER   setContainerHidden  p�      ��      �  ~  b	      LOGICAL,INPUT plHidden LOGICAL  setContainerSource  ̐      �      @�    u	      LOGICAL,INPUT phObject HANDLE   setContainerSourceEvents     �      `�      ��  �  �	      LOGICAL,INPUT pcEvents CHARACTER    setDataLinksEnabled |�      ��      ��  �  �	      LOGICAL,INPUT lDataLinksEnabled LOGICAL setDataSource   ԑ      �      L�  �  �	      LOGICAL,INPUT phObject HANDLE   setDataSourceEvents ,�      l�      ��  �  �	      LOGICAL,INPUT pcEventsList CHARACTER    setDataSourceNames  ��      Ȓ      ��  �  �	      LOGICAL,INPUT pcSourceNames CHARACTER   setDataTarget   ܒ      $�      T�  �  �	      LOGICAL,INPUT pcTarget CHARACTER    setDataTargetEvents 4�      x�      ��  �  �	      LOGICAL,INPUT pcEvents CHARACTER    setDBAware  ��      Г      ��  � 
 
      LOGICAL,INPUT lAware LOGICAL    setDesignDataObject ܓ      �      P�  �  
      LOGICAL,INPUT pcDataObject CHARACTER    setDynamicObject    0�      x�      ��  �  +
      LOGICAL,INPUT lTemp LOGICAL setInstanceProperties   ��      Ȕ       �  �  <
      LOGICAL,INPUT pcPropList CHARACTER  setLogicalObjectName    ��      $�      \�  �  R
      LOGICAL,INPUT c CHARACTER   setLogicalVersion   <�      x�      ��  �  g
      LOGICAL,INPUT cVersion CHARACTER    setObjectName   ��      Е       �  �  y
      LOGICAL,INPUT pcName CHARACTER  setObjectParent ��       �      P�  �  �
      LOGICAL,INPUT phParent HANDLE   setObjectVersion    0�      p�      ��  �  �
      LOGICAL,INPUT cObjectVersion CHARACTER  setParentDataKey    ��      ̖       �  �  �
      LOGICAL,INPUT cParentDataKey CHARACTER  setPassThroughLinks ��      (�      \�  �  �
      LOGICAL,INPUT pcLinks CHARACTER setPhysicalObjectName   <�      |�      ��  �  �
      LOGICAL,INPUT cTemp CHARACTER   setPhysicalVersion  ��      ԗ      �  �  �
      LOGICAL,INPUT cVersion CHARACTER    setRunAttribute �      ,�      \�  �  �
      LOGICAL,INPUT cRunAttribute CHARACTER   setSupportedLinks   <�      ��      ��  �        LOGICAL,INPUT pcLinkList CHARACTER  setTranslatableProperties   ��      ܘ      �  �        LOGICAL,INPUT pcPropList CHARACTER  setUIBMode  ��      <�      h�  � 
 2      LOGICAL,INPUT pcMode CHARACTER  setUserProperty H�      ��      ��  �  =      LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER  showmessage ��      ��      $�  �  M      LOGICAL,INPUT pcMessage CHARACTER   Signature   �      H�      t�  � 	 Y      CHARACTER,INPUT pcName CHARACTER    l�      ��  0�      �       4   �����                 @�                      ��                    G                  $�e	                         Ě          \�  ؛      �       4   �����                 �                      ��                    F                  ��e	                         l�  �    3  �  ��      �       4   �����                 ��                      ��                  ?  A                  ,�e	                       ?  �         @                                  ,     
                    � ߱        �  $  C  ��  ���                           $  E  @�  ���                       x                         � ߱        x�    K  ��  �      �      4   �����                �                      ��                  L  	                  �e	                       L  ��  H�  o   O      ,                                 ��  $   P  t�  ���                       �  @         �              � ߱        ��  �   Q        Ȟ  �   R  �      ܞ  �   T        �  �   V  x      �  �   X  �      �  �   Z  `      ,�  �   [  �      @�  �   \        T�  �   _  �      h�  �   a         |�  �   b  |      ��  �   d  �      ��  �   e  t      ��  �   f  �      ̟  �   g  ,      ��  �   h  �      ��  �   n  �      �  �   p  P	      �  �   v  �	      0�  �   x   
      D�  �   z  t
      X�  �   {  �
      l�  �   �  l      ��  �   �  �      ��  �   �  \      ��  �   �  �      ��  �   �  D      Р  �   �  �      �  �   �  �      ��  �   �  0      �  �   �  �       �  �   �  �      4�  �   �        H�  �   �  X      \�  �   �  �      p�  �   �        ��  �   �  L      ��  �   �  �      ��  �   �  �      ��  �   �         ԡ  �   �  <      �  �   �  x      ��  �   �  �      �  �   �  �          �   �  ,                      <�          ��  ��      ��                  7	  e	  ��              ��e	                    O   ����    e�          O   ����    R�          O   ����    ��      �     
                                     (                         � ߱        h�  $ K	  آ  ���                           O   c	  ��  ��  h               ԣ          ģ  ̣    ��                                             ��                            ����                                8;      $�      ��     V     ܣ                       أ  a                     8�    �	  ��  �      t      4   ����t                 �                      ��                  �	  
                  ��e	                       �	  ��  4�  �   �	  �      H�  �   �	  H      \�  �   �	  �      p�  �   �	  @      ��  �   �	  �      ��  �   �	  8      ��  �   �	  �      ��  �   �	  (      ԥ  �   �	  �      �  �   �	         ��  �   �	  �      �  �   �	        $�  �   �	  �          �   �	        ��    <
  T�  Ц      x      4   ����x                �                      ��                  =
  �
                  ��e	                       =
  d�  ��  �   ?
  �      �  �   @
  T      �  �   A
  �      0�  �   B
  D      D�  �   C
  �      X�  �   D
  �      l�  �   F
  p      ��  �   G
  �      ��  �   H
  X      ��  �   I
  �      ��  �   J
  �      Ч  �   K
  D       �  �   L
  �       ��  �   M
  �       �  �   N
  x!       �  �   O
  �!      4�  �   P
  h"      H�  �   Q
  �"      \�  �   R
  `#      p�  �   S
  �#      ��  �   T
  X$      ��  �   U
  �$      ��  �   V
  �$      ��  �   W
  L%      Ԩ  �   X
  �%      �  �   Y
  <&      ��  �   Z
  �&      �  �   [
  4'      $�  �   \
  �'      8�  �   ]
  ,(      L�  �   ^
  h(      `�  �   `
  �(      t�  �   a
  X)      ��  �   b
  �)      ��  �   c
  *      ��  �   d
  �*      ĩ  �   e
  �*      ة  �   f
  l+      �  �   g
  �+       �  �   h
  \,      �  �   i
  �,      (�  �   j
  L-      <�  �   k
  �-      P�  �   l
  <.      d�  �   m
  �.      x�  �   n
  4/      ��  �   o
  �/          �   p
  $0      d�    �
  ��  8�      T0      4   ����T0                H�                      ��                  �
  y                  ��e	                       �
  ̪  \�  �   �
  �0      p�  �   �
  (1      ��  �   �
  �1      ��  �   �
  2      ��  �   �
  �2      ��  �   �
  3      ԫ  �   �
  |3      �  �   �
  �3      ��  �   �
  t4      �  �   �
  �4      $�  �   �
  l5      8�  �   �
  �5      L�  �   �
  d6      `�  �   �
  �6      t�  �   �
  L7      ��  �   �
  �7      ��  �   �
  <8      ��  �   �
  �8      Ĭ  �      ,9      ج  �     �9      �  �     :       �  �     X:      �  �     �:      (�  �     H;      <�  �     �;      P�  �     8<          �     �<      x�    �  ��  ��      =      4   ����=  	              �                      ��             	     �  (                  ��d	                       �  ��   �  �   �  |=      4�  �   �  �=      H�  �   �  t>      \�  �   �  �>      p�  �   �  l?      ��  �   �  �?      ��  �   �  \@      ��  �   �  �@      ��  �   �  TA      Ԯ  �   �  �A      �  �   �  DB      ��  �   �  �B      �  �   �  <C      $�  �   �  �C      8�  �   �  ,D      L�  �   �  �D      `�  �   �  $E      t�  �   �  �E      ��  �   �  F      ��  �   �  �F      ��  �   �  G      į  �   �  �G      د  �   �  �G      �  �   �  8H       �  �   �  �H      �  �   �  0I      (�  �   �  �I      <�  �   �   J          �   �  �J      getRowObjUpdStatic  deleteRecordStatic  �    �  ��  ��      K      4   ����K      /   �  а     �                          3   ����K             �                      3   ����<K  Զ    �  ,�  ��  �  XK      4   ����XK  
              ��                      ��             
     �  J                  f	                       �  <�  ̱  �   �  �K      $�  $  �  ��  ���                       �K     
                    � ߱        8�  �   �  L      ��  $   �  d�  ���                       ,L  @         L              � ߱        L�  $  �  ��  ���                       �L       	       	           � ߱        �M     
                N                     \O  @        
 O              � ߱        ܳ  V   �  �  ���                        hO       	       	       �O       
       
       �O       	       	           � ߱        l�  $    x�  ���                       �P     
                Q                     dR  @        
 $R              � ߱            V   +  �  ���                                      ̵                      ��                  L  �                  pf	                       L  ��  pR     
                �R                     <T  @        
 �S          �T  @        
 dT          U  @        
 �T          dU  @        
 $U              � ߱            V   a  �  ���                        adm-clone-props �  ��              �     W     `                          \  Z                     start-super-proc    �  d�  �           �     X                                  {                     l�      �   �      �X      4   �����X      /     ,�     <�                          3   ���� Y            \�                      3   ���� Y  ķ  $    ��  ���                       @Y                         � ߱        ��    ,  �  \�  ��  \Y      4   ����\Y                и                      ��                  -  1                  �f	                       -  �  pY                     �Y                     �Y                         � ߱            $  .  l�  ���                             2  �  T�      �Y      4   �����Y  �Y                         � ߱            $  3  (�  ���                       |�    :  ��  ��  �  �Y      4   �����Y      $  ;  ع  ���                       Z                         � ߱            �   X  Z      XZ     
                �Z                     $\  @        
 �[              � ߱        ��  V   l  �  ���                        ��  �   �  0\      ��      غ  �      p\      4   ����p\      /     �     $�                          3   �����\            D�                      3   �����\  �\     
                <]                     �^  @        
 L^              � ߱        H�  V   +  T�  ���                        �^     
                T_                     �`  @        
 d`              � ߱        t�  V   O  �  ���                        ��    �  ��  �      �`      4   �����`                �                      ��                  �  �                  �Hf	                       �  ��  ��  /   �  H�     X�                          3   �����`            x�                      3   �����`      /   �  ��     Ľ                          3   ����a            �                      3   ����$a   �  /  <   �         Xa                      3   ����@a  initProps   x�  0�                   Y     �             �          �  �  	                                   t�          �  �      ��                �  �  4�              p�f	                    O   ����    e�          O   ����    R�          O   ����    ��                            L�          ��  p   �  (|  ��      �  ��  �     4|                �                      ��                  �  �                  �f	                       �  ��  4�  :  �                 $  �  `�  ���                       H|                         � ߱        �  �     `|                                        ��                  �  �                  ��f	                       �  ��  ��  ��     t|                                        ��                  �                     T�f	                       �  (�  0�   �     �|                                        ��                                      $�f	                         ��  ��  ��     �|                                        ��                    :                  4�f	                         @�  H�  8�     �|                                        ��                  ;  W                  �f	                       ;  ��  ��  ��     �|                                        ��                  X  t                  ��f	                       X  X�  `�  P�     �|                                        ��                  u  �                  ��f	                       u  ��  ��  ��     �|  	                                      ��             	     �  �                  t�f	                       �  p�  x�  h�      }  
                                      ��             
     �  �                  ��f	                       �  ��  �  ��     }                                        ��                  �  �                  P�f	                       �  ��  ��  ��     (}                                        ��                  �                     �f	                       �  �  �  �     <}                                        ��                    "                  ��f	                         ��  ��  ��     P}                                        ��                  #  ?                  4�f	                       #  ,�  4�  $�     d}                                        ��                  @  \                  ��f	                       @  ��  ��  ��     x}                                        ��                  ]  y                  ��f	                       ]  D�  L�  <�     �}                                        ��                  z  �                  h�f	                       z  ��      ��     �}                                        ��                  �  �                  8�f	                       �  \�      O   �  ��  ��  �}               \�          D�  P�   , $�                                                       �     ��                            ����                            <�  d�  X�  ��      ��     Z     d�                      � `�                       ��    �  �  ��      �}      4   �����}                ��                      ��                  �  �                  ��f	                       �  ,�  �  /   �  ��     ��                          3   �����}            �                      3   �����}  ��  /   �  @�     P�                          3   ����~            p�                      3   ����(~  ��  /   �  ��     ��                          3   ����D~            ��                      3   ����d~      /   �  �     (�                          3   �����~            H�                      3   �����~  �~     
                @                     ��  @        
 P�              � ߱        ��  V   0  X�  ���                        ��  $  J  �  ���                       ��                         � ߱        Ā     
                @�                     ��  @        
 P�              � ߱        ��  V   T  @�  ���                        ��  $  n  ��  ���                       ��     
                    � ߱        ��     
                ,�                     |�  @        
 <�              � ߱        ��  V   x  (�  ���                        t�  $  �  ��  ���                       ��     
                    � ߱        ��     
                �                     h�  @        
 (�              � ߱        ��  V   �  �  ���                        \�  $  �  ��  ���                       ��                         � ߱        ��     
                $�                     t�  @        
 4�              � ߱        ��  V   �  ��  ���                        ��  �   �  ��      X�  $  �  ��  ���                       ��     
                    � ߱        ��     
                <�                     ��  @        
 L�              � ߱        ��  V   �  ��  ���                        ��  $     ��  ���                       ��     
                    � ߱        ��  �     ��      H�  $  $  �  ���                       �     
                    � ߱        \�  �   >   �      ��  $  `  ��  ���                       @�                         � ߱              k  ��  ��      \�      4   ����\�      /   l  �     �                          3   ����|�  L�     
   <�                      3   ������  |�        l�                      3   ������  ��        ��                      3   ������            ��                      3   ����ԋ  pushRowObjUpdTable  ��  ��  �                   [      �                               R                     pushTableAndValidate    ��  L�  �           |     \     �                          �  o                     remoteCommit    d�  ��  �           p     ]     �                          �  �                     serverCommit    ��  ,�  �           l     ^     �                          �  �                                     L�          �  �      ��                  �  �  4�              (g	                    O   ����    e�          O   ����    R�          O   ����    ��          O   �  ��  ��  �    ��                            ����                            <�  P�      ��              _      d�                      
�     �                     disable_UI  ��   �                      `      �                               �  
                    �  �    ����  �       ��          ��  8   ����   ��  8   ����   ��  8   ����   ��  8   ����       8   ����       8   ����        �  �      viewObject  ,   ��   �  ,�      toggleData  ,INPUT plEnabled LOGICAL    �  X�  p�      showMessageProcedure    ,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL  H�  ��  ��      returnFocus ,INPUT hTarget HANDLE   ��  ��  ��      repositionObject    ,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL    ��  8�  D�      removeLink  ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE (�  ��  ��      removeAllLinks  ,   ��  ��  ��      modifyUserLinks ,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE ��  $�  8�      modifyListProperty  ,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER    �  ��  ��      hideObject  ,   ��  ��  ��      exitObject  ,   ��  ��  �      editInstanceProperties  ,   ��  �  ,�      displayLinks    ,   �  @�  P�      createControls  ,   0�  d�  t�      changeCursor    ,INPUT pcCursor CHARACTER   T�  ��  ��      applyEntry  ,INPUT pcField CHARACTER    ��  ��  ��      adjustTabOrder  ,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER ��  @�  L�      addMessage  ,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER 0�  ��  ��      addLink ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE ��   �  �      unbindServer    ,INPUT pcMode CHARACTER ��  8�  H�      runServerObject ,INPUT phAppService HANDLE  (�  t�  ��      disconnectObject    ,   d�  ��  ��      destroyObject   ,   ��  ��  ��      bindServer  ,   ��  ��  ��      transferDBRow   ,INPUT pcRowIdent CHARACTER,INPUT piRowNum INTEGER  ��  4�  @�      startFilter ,   $�  T�  d�      releaseDBRow    ,   D�  x�  ��      refetchDBRow    ,INPUT phRowObjUpd HANDLE   h�  ��  ��      filterContainerHandler  ,INPUT phFilterContainer HANDLE ��  ��  �      fetchDBRowForUpdate ,   ��  $�  4�      confirmContinue ,INPUT-OUTPUT pioCancel LOGICAL �  d�  t�      compareDBRow    ,   T�  ��  ��      bufferCopyDBToRO    ,INPUT phRowObj HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER   x�  �  �      assignDBRow ,INPUT phRowObjUpd HANDLE    �  H�  T�      updateState ,INPUT pcState CHARACTER    8�  ��  ��      updateQueryPosition ,   p�  ��  ��      updateAddQueryWhere ,INPUT pcWhere CHARACTER,INPUT pcField CHARACTER    ��   �  �      undoTransaction ,   ��  $�  4�      transferToExcel ,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT plUseExisting LOGICAL,INPUT piMaxRecords INTEGER  �  ��  ��      synchronizeProperties   ,INPUT pcPropertiesForServer CHARACTER,OUTPUT pcPropertiesForClient CHARACTER   ��  ,�  @�      submitValidation    ,INPUT pcValueList CHARACTER,INPUT pcUpdColumns CHARACTER   �  ��  ��      submitForeignKey    ,INPUT pcRowIdent CHARACTER,INPUT-OUTPUT pcValueList CHARACTER,INPUT-OUTPUT pcUpdColumns CHARACTER  |�  �  $�      submitCommit    ,INPUT pcRowIdent CHARACTER,INPUT plReopen LOGICAL  �  h�  |�      startServerObject   ,   X�  ��  ��      setPropertyList ,INPUT pcProperties CHARACTER   ��  ��  ��      serverFetchRowObjUpdTable   ,OUTPUT TABLE-HANDLE phRowObjUpd    ��   �  0�      serverSendRows  ,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER,OUTPUT TABLE-HANDLE phRowObject    �  ��   �      saveContextAndDestroy   ,OUTPUT pcContext CHARACTER ��  ,�  <�      rowObjectState  ,INPUT pcState CHARACTER    �  h�  x�      retrieveFilter  ,   X�  ��  ��      restartServerObject ,   |�  ��  ��      remoteSendRows  ,INPUT-OUTPUT piocContext CHARACTER,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT pioRowsReturned INTEGER,OUTPUT TABLE-HANDLE phRowObject,OUTPUT pocMessages CHARACTER   ��  ��  ��      refreshRow  ,   ��  ��  ��      printToCrystal  ,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT piMaxRecords INTEGER  ��  P�  `�      isUpdatePending ,INPUT-OUTPUT plUpdate LOGICAL  @�  ��  ��      initializeServerObject  ,   ��  ��  ��      initializeObject    ,   ��  ��  ��      home    ,   ��   �  �      genContextList  ,OUTPUT pcContext CHARACTER ��  <�  H�      fetchPrev   ,   ,�  \�  h�      fetchNext   ,   L�  |�  ��      fetchLast   ,   l�  ��  ��      fetchFirst  ,   ��  ��  ��      fetchBatch  ,INPUT plForwards LOGICAL   ��  ��  �      endClientDataRequest    ,   ��   �  4�      destroyServerObject ,   �  H�  X�      describeSchema  ,INPUT pcSdoName CHARACTER,OUTPUT TABLE-HANDLE hTtSchema    8�  ��  ��      dataAvailable   ,INPUT pcRelative CHARACTER ��  ��  ��      copyColumns ,INPUT pcViewColList CHARACTER,INPUT phDataQuery HANDLE ��  4�  H�      commitTransaction   ,   $�  \�  l�      clientSendRows  ,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER    L�  �  �      batchServices   ,INPUT pcServices CHARACTER,OUTPUT pcValues CHARACTER        � 
"     
 �%     adecomm/as-utils.w 
"   
   �    }        �
"     
    �     }        ��    :   %               � 
" 	   
 �%              h �P  \         (          
�                          
�            � c   �
" 	   
 e	
�H T   %              �     }        �GG %              � 
"   
   P �L 
�H T   %              �     }        �GG %              
"   
   �        �    7%               
"   
 �               1� s  
 � ~   �%               o%   o           � �    
"   
 �           �    1� �   � ~   �%               o%   o           � �   
"   
 �           �    1� �  
 � ~   �%               o%   o           � �   
"   
 �           l    1� �   � ~   �%               o%   o           � �    
"   
 �           �    1� �   � ~   �%               o%   o           � �   
"   
 �           T    1� �   � �   �%               o%   o           %               
"   
 ��          �    1� �   �� 
     
"   
 �               1�    � ~   �%               o%   o           � $  
"   
 �           �    1� &   � ~   �%               o%   o           � 5  S 
"   
 �           �    1� �   � �   �%               o%   o           %               
"   
 �           p    1� �   � �   �%               o%   o           %               
"   
 �           �    1� �   � �   �%               o%   o           %              
"   
 ��          h    1� �   �� �     
"   
 �           �    1� �  
 � �   �%               o%   o           %               
"   
 �                1� �   � ~   �%               o%   o           � �    
"   
 ��          �    1� �   �� 
     
"   
 �           �    1� �   � ~   �%               o%   o           �    t 
"   
 ��          D	    1� u  
 �� 
     
"   
 �           �	    1� �   � ~   �%               o%   o           � �  � 
"   
 �           �	    1�    � ~   �%               o%   o           � �    
"   
 �           h
    1� 5  
 � @   �%               o%   o           %               
"   
 e	�           �
    1� D   e	� �   �%               o%   o           %              
"   
 e	�           `    1� L   e	� ~   �%               o%   o           � �    e	
"   
 e	�           �    1� ]   e	� ~   �%               o%   o           o%   o           
"   
 e	�           P    1� m  
 e	� ~   �%               o%   o           � �    e	
"   
 e	�           �    1� x   e	� �  	 �%               o%   o           � �  / e	
"   
 ��          8    1� �   �� �  	   
"   
 e	�           t    1� �   e	� �  	 �o%   o           o%   o           � �    e	
"   
 ��          �    1� �   �� �  	   
"   
 e	�           $    1� �   e	� �  	 �o%   o           o%   o           � �    e	
"   
 ��          �    1�    �� �     
"   
 ��          �    1�    �� �  	   
"   
 ��              1� "   �� �  	   
"   
 ��          L    1� /   �� �  	   
"   
 e	�           �    1� =   e	� �   �o%   o           o%   o           %              
"   
 ��              1� N   �� �  	   
"   
 ��          @    1� \  
 �� g     
"   
 ��          |    1� o   �� �  	   
"   
 ��          �    1� ~   �� �  	   
"   
 ��          �    1� �   �� �  	   
"   
 ��          0    1� �   �� �  	   
"   
 ��          l    1� �  	 �� �  	   
"   
 ��          �    1� �   �� �  	   
"   
 ��          �    1� �   �� �  	   
"   
 e	�                1� �   e	� ~   �%               o%   o           o%   o           
�H T   %              �     }        �GG %              
"   
   
"   
 e	
"   
   
"   
 �(�  L ( l       �        �    �� �   � P   �        �    �@    
� @  , 
�            �� �     p�               �L
�    %              � 8          � $         �           
�    �      
"   
 �� @  , 
�           �� �  
 �p�               �L"      P �L 
�H T   %              �     }        �GG %              
"   
 e	�           �    1� "  
 e	� ~   �%               o%   o           � �    e	
"   
 e	�           <    1� -  
 e	� ~   �%               o%   o           o%   o           
"   
 e	�           �    1� 8   e	� 
   �%               o%   o           o%   o           
"   
 e	�           4    1� A   e	� �   �%               o%   o           %               
"   
 e	�           �    1� P   e	� �   �%               o%   o           %               
"   
 ��           ,    1� ]   �� ~   �%               o%   o           � �    e	
"   
 e	�           �    1� d   e	� �   �%               o%   o           %              
"   
 e	�               1� v   e	� �   �%               o%   o           o%   o           
"   
 e	�           �    1� �   e	� ~   �%               o%   o           o%   o           
"   
 e	�               1� �  	 e	� ~   �%               o%   o           � �    e	
"   
 e	�           �    1� �   e	� ~   �%               o%   o           o%   o           
"   
 e	�               1� �   e	� ~   �%               o%   o           o%   o           
"   
 e	�           �    1� �   e	� �   �%               o%   o           %               
"   
 e	�           �    1� �   e	� �   �%               o%   o           o%   o           P �L 
�H T   %              �     }        �GG %              
"   
 e	�           �    1� �  
 e	� �   �%               o%   o           %              
"   
 e	�           H    1� �   e	� ~   �%               o%   o           o%   o           
"   
 e	�           �    1� �   e	� ~   �%               o%   o           � �    e	
"   
 e	�           8    1� �   e	� ~   �%               o%   o           o%   o           
"   
 ��          �    1� 
   �� 
     
"   
 e	�           �    1�    e	� ~   �%               o%   o           � *  ! e	
"   
 e	�           d    1� L   e	� ~   �%               o%   o           � �    e	
"   
 e	�           �    1� Y   e	� ~   �%               o%   o           � l   e	
"   
 ��          L    1� {   �� �     
"   
 ��          �    1� �   �� 
     
"   
 e	�           �    1� �   e	� ~   �%               o%   o           � �    e	
"   
 ��          8     1� �  
 �� 
     
"   
 ��           t     1� �   �� �   �%               o%   o           o%   o           
"   
 e	�           �     1� �   e	� �   �%               o%   o           %               
"   
 e	�           l!    1� �   e	� �   �%               o%   o           %               
"   
 e	�           �!    1� �   e	� ~   �%               o%   o           � �    e	
"   
 e	�           \"    1� �   e	� ~   �%               o%   o           o%   o           
"   
 e	�           �"    1�    e	� �   �%               o%   o           %              
"   
 e	�           T#    1�    e	� �   �%               o%   o           %               
"   
 ��           �#    1�    �� �   �%               o%   o           %               
"   
 ��          L$    1� /   �� 
     
"   
 ��          �$    1� <   �� ~     
"   
 e	�           �$    1� I   e	� @   �%               o%   o           o%   o           
"   
 e	�           @%    1� U   e	� ~   �%               o%   o           � �    e	
"   
 e	�           �%    1� c   e	� ~   �%               o%   o           o%   o           
"   
 e	�           0&    1� q   e	� �   �o%   o           o%   o           o%   o           
"   
 e	�           �&    1� �   e	� �  	 �%               o%   o           o%   o           
"   
 e	�           ('    1� �   e	� ~   �%               o%   o           o%   o           
"   
 e	�           �'    1� �  
 e	� @   �%               o%   o           o%   o           
"   
 ��           (    1� �   �� ~     
"   
 e	�           \(    1� �   e	� ~   �%               o%   o           � �  4 e	
"   
 e	�           �(    1�   
 e	� �   �%               o%   o           %              
"   
 ��          L)    1�    �� 
     
"   
 e	�           �)    1� (   e	� ~   �%               o%   o           � �    �
"   
 e	�           �)    1� 6   e	� �   �%               o%   o           %              
"   
 e	�           x*    1� E   e	� ~   �%               o%   o           � �    e	
"   
 e	�           �*    1� R   e	� ~   �%               o%   o           � �    e	
"   
 e	�           `+    1� `   e	� ~   �%               o%   o           � �    e	
"   
 e	�           �+    1� l   e	� �   �%               o%   o           %               
"   
 e	�           P,    1� {  	 e	� 
   �%               o%   o           o%   o           
"   
 ��           �,    1� �   �� ~   �%               o%   o           � �  	 e	
"   
 e	�           @-    1� �   e	� @   �%               o%   o           %       �       
"   
 e	�           �-    1� �   e	� ~   �%               o%   o           � �    e	
"   
 e	�           0.    1� �   e	� �   �o%   o           o%   o           %              
"   
 e	�           �.    1� �   e	� �   �%               o%   o           %               
"   
 e	�           (/    1� �   e	� ~   �%               o%   o           o%   o           
"   
 e	�           �/    1� �   e	� �  	 �%               o%   o           � �    e	
"   
 ��          0    1� �   �� �  	   P �L 
�H T   %              �     }        �GG %              
"   
 ��           �0    1� 	  
 �� ~   �%               o%   o           � �    �
"   
 e	�           1    1�    e	� �   �%               o%   o           %               
"   
 e	�           �1    1� !  	 e	� ~   �%               o%   o           � �    e	
"   
 e	�           2    1� +   e	� ~   �%               o%   o           � �    e	
"   
 e	�           �2    1� 9   e	� �   �%               o%   o           %               
"   
 e	�           �2    1� I   e	� ~   �%               o%   o           � �    e	
"   
 e	�           p3    1� \   e	� ~   �%               o%   o           o%   o           
"   
 e	�           �3    1� d   e	� ~   �%               o%   o           o%   o           
"   
 e	�           h4    1� q   e	� �   �%               o%   o           o%   o           
"   
 ��           �4    1�    �� �   �%               o%   o           o%   o           
"   
 e	�           `5    1� �   e	� �   �%               o%   o           o%   o           
"   
 e	�           �5    1� �   e	� ~   �%               o%   o           o%   o           
"   
 e	�           X6    1� �  	 e	� �  	 �%               o%   o           � �    e	
"   
 e	�           �6    1� �  
 e	� �  	 �%               o%   o           � �    e	
"   
 e	�           @7    1� �   e	� ~   �%               o%   o           � �    e	
"   
 e	�           �7    1� �   e	� ~   �%               o%   o           o%   o           
"   
 e	�           08    1� �   e	� ~   �%               o%   o           o%   o           
"   
 e	�           �8    1� �   e	� ~   �%               o%   o           � �    e	
"   
 e	�            9    1�    e	� ~   �%               o%   o           � �    e	
"   
 e	�           �9    1�    e	� �  	 �%               o%   o           o%   o           
"   
 ��          :    1� $   �� 
     
"   
 e	�           L:    1� 0   e	� ~   �%               o%   o           � �    e	
"   
 e	�           �:    1� >   e	� ~   �%               o%   o           o%   o           
"   
 ��           <;    1� Q   �� �   �%               o%   o           o%   o           
"   
 e	�           �;    1� c  
 e	� ~   �%               o%   o           � �    e	
"   
 e	�           ,<    1� n   e	� ~   �%               o%   o           � �    e	
"   
 e	�           �<    1� �   e	� �   �%               o%   o           %               P �L 
�H T   %              �     }        �GG %              
"   
 e	�           p=    1� �  	 e	� 
   �%               o%   o           o%   o           
"   
 e	�           �=    1� �   e	� 
   �%               o%   o           o%   o           
"   
 e	�           h>    1� �   e	� 
   �%               o%   o           o%   o           
"   
 ��           �>    1� �   �� �   �%               o%   o           %              
"   
 e	�           `?    1� �   e	� ~   �%               o%   o           � �  M �
"   
 e	�           �?    1� :   e	� �   �%               o%   o           %              
"   
 e	�           P@    1� K   e	� �   �%               o%   o           %               
"   
 e	�           �@    1� _   e	� �   �%               o%   o           %               
"   
 e	�           HA    1� v   e	� �  	 �%               o%   o           � �   e	
"   
 e	�           �A    1� �   e	� �   �%               o%   o           %               
"   
 e	�           8B    1� �   e	� �  	 �%               o%   o           o%   o           
"   
 e	�           �B    1� �   e	� �   �o%   o           o%   o           %              
"   
 ��           0C    1� �   �� �  	 �o%   o           o%   o           � �    �
"   
 e	�           �C    1� �   e	� 
   �o%   o           o%   o           o%   o           
"   
 e	�            D    1� �   e	� 
   �o%   o           o%   o           o%   o           
"   
 e	�           �D    1� �   e	� �  	 �o%   o           o%   o           o%   o           
"   
 e	�           E    1�    e	� 
   �o%   o           o%   o           o%   o           
"   
 e	�           �E    1�    e	� �  	 �o%   o           o%   o           � "   e	
"   
 e	�           F    1� $   e	� �  	 �o%   o           o%   o           � 3   e	
"   
 e	�           |F    1� ?   e	� �   �%               o%   o           %               
"   
 e	�           �F    1� S   e	� �   �%               o%   o           %               
"   
 ��          tG    1� g   �� �  	   
"   
 e	�           �G    1� {   e	� �   �%               o%   o           %               
"   
 e	�           ,H    1� �   e	� ~   �%               o%   o           o%   o           
"   
 e	�           �H    1� �   e	� ~   �%               o%   o           o%   o           
"   
 e	�           $I    1� �   e	� �   �%               o%   o           o%   o           
"   
 e	�           �I    1� �   e	� ~   �%               o%   o           � �    e	
"   
 e	�           J    1� �   e	� �   �%               o%   o           %               
"   
 e	�           �J    1� �  	 e	� �   �%               o%   o           %                "    �%     start-super-proc u�%     adm2/smart.p ��P �L 
�H T   %              �     }        �GG %              
"   
   �       �K    6� �     
"   
   
�        �K    8
"   
   �        �K    ��     }        �G 4              
"   
 ߱G %              G %              %�   AppService,ASInfo,ASUsePrompt,CacheDuration,CheckCurrentChanged,DestroyStateless,DisconnectAppServer,ServerOperatingMode,ShareData,UpdateFromSource,ForeignFields,ObjectName,OpenOnInit,PromptColumns,PromptOnDelete,RowsToBatch,RebuildOnRepos,ToggleDataTargets  
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        �M    �� �   � P   �        �M    �@    
� @  , 
�       �M    �� �   �p�               �L
�    %              � 8       N    � $         �           
�    �    �
"   
 �p� @  , 
�       O    ��    �p�               �L"  	  , �   �    e	�    ��     }        �A      |    "  	    �    e	%              (<   \ (    |    �     }        �A�     �A"  
  e	    "  	  �"  
  e	  < "  	  �"  
  e	(    |    �     }        �A�     �A"  
  e	
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        �P    �� �   � P   �        �P    �@    
� @  , 
�       �P    �� �   �p�               �L
�    %              � 8      Q    � $         �           
�    �    �
"   
 �p� @  , 
�       R    �� s  
 �p�               �L"  	  , 
�H T   %              �     }        �GG %              
"   
   
"   
 f	
"   
   
"   
   (�  L ( l       �        �R    �� �   � P   �        �R    �@    
� @  , 
�       �R    �� �     p�               �L
�    %              � 8      �R    � $         �           
�    �      
"   
 �p� @  , 
�       �S    �� �  
 �p�               �L%     SmartDataObject 
"   
   p� @  , 
�       XT    �� �     p�               �L%               
"   
  p� @  , 
�       �T    �� �    p�               �L%               
"   
  p� @  , 
�       U    �� �    p�               �L(        � �      � �      � �      �     }        �A
�H T   %              �     }        �GG %              
"   
 e	 (   � 
"   
 �    �        �U    �� �   �
"   
   � 8      DV    � $         �           
�    �    �
"   
   �        �V    �
"   
   �       �V    /
"   
   
"   
   �       �V    6� �     
"   
   
�        W    8
"   
   �        4W    �
"   
   �       TW    �
"   
   p�    � I   e	
�    �     }        �G 4              
"   
 ߱G %              G %              
�     }        �
"   
    (   � 
"   
 �    �        X    �A"    �A
"   
   
�        dX    �@ � 
"   
 e	"      �       }        �
"   
 �%              %                "    �%     start-super-proc u�%     adm2/appserver.p �e	�    � �     
�    �     }        �%               %      Server  - �     }        �    "    e	� �    �%                   "    e	� �    �%      NONE    p�,  8         $     "    e	        � �   �
�    
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        �Z    �� �   � P   �        �Z    �@    
� @  , 
�       �Z    �� �   �p�               �L
�    %              � 8      �Z    � $         �           
�    �    �
"   
 �p� @  , 
�       �[    �� �   �p�               �L"    , p�,  8         $     "    e	        � �   �
�     "    �%     start-super-proc t�%     adm2/dataquery.p -�
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
 �(�  L ( l       �        ]    �� �   � P   �        ]    �@    
� @  , 
�       $]    �� �   �p�               �L
�    %              � 8      0]    � $         �    �     
�    �    �
"   
 �p� @  , 
�       @^    �� �   �p�               �L%H > 8   dataAvailable,confirmContinue,isUpdatePending,buildDataRequest  
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
 �(�  L ( l       �        $_    �� �   � P   �        0_    �@    
� @  , 
�       <_    �� �   �p�               �L
�    %              � 8      H_    � $         �    �     
�    �    �
"   
 �p� @  , 
�       X`    �� �   �p�               �L%               "    �%     start-super-proc s�%     adm2/query.p ��%     start-super-proc s�%     adm2/queryext.p % 	    initProps �
�    %4 + $   FOR EACH Almcmov NO-LOCK INDEXED-REPOSITION �   � _     � a     �       
�     	         �G
"   
 e	�        �a    �G
"   
   
"   
    x    (0 4      �        �a    �G%                   �        b    �GG %              � E    �� F         %              %                   "      %              
"   
       "      �        �b    �
"   
   �        (c    �
"   
   
�       Hc    �"       \      H   "    �((       "      %              � �      � _   �     
"   
   
"   
 � \      H   "      ((       "      %              � �     � _   e	�        �c    �%                   %              %                   "  (    %                  "  (        
"   
 �
"   
 e	0 T       m � "  (  ��        �d    �A @   "      $         � "  (  e	�     ��        e    �� "  (    
"   
 � \ H     H   "      ((       "    �%              � �    �� _     (        "  !  �� �    e	�        �e    �"  !  �
�H T   %              �     }        �GG %              
"   
 f	
"   
 f	
"   
   
"   
   (�  L ( l       �        �f    �� �   � P   �        �f    �@    
� @  , 
�       �f    �� �     p�               �L
�    %              � 8      �f    � $         �           
�    �      
"   
 �p� @  , 
�       �g    �� �   �p�               �L%               
"   
   p� @  , 
�       Dh    �� �     p�               �L"    , �,  8         $     "    �L        � M  
  
�    
�H T   %              �     }        �GG %              
"   
 $ 
"   
   
"   
 �
"   
 �(�  L ( l       �        (i    �� �   � P   �        4i    �@    
� @  , 
�       @i    �� �   �p�               �L
�    %              � 8      Li    � $         �    �     
�    �      
"   
 �p� @  , 
�       \j    �� $   �p�               �L
"   
 , 
"   
   p� @  , 
�       �j    ��      p�               �L"    , 
"   
  p� @  , 
�       k    �� �    p�               �L"    ,     "    e	� �    �%L B <   OPEN QUERY Query-Main FOR EACH Almcmov NO-LOCK INDEXED-REPOSITION.     "    cm� �    I((        "    OS%                   "    �� �     "    � (   "           "    �%              @ �,  8         $     "    �        � �    
�    p�,  8         $     � �   f	        � �   �
�    %               �    "      � a         %              %                   "      %                  "      "      "     T(        "    f	%              "    f	� a   �"      �       "    ��    "    f	�     �� �      �     ��    "     �      S    "      "    �    "    f	%                � @    �     t T     P   4       �"      (0       4       e	"      � �      � �    �� _   e	T ,  %              T   "    e	"    �� a     �     �� _   e	T    �    "    e	�     �"      �     �"      %                   %              %                   "      %                  "      �     "      �     "       \      H   "      ((       "    �%              � �    �� �     4  e	     "      
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        �q    �� �   � P   �        �q    �@    
� @  , 
�       �q    �� �   �p�               �L
�    %              � 8      �q    � $         �           
�    �    �
"   
 �p� @  , 
�       �r    �� 	  
 �p�               �L"    ,       "  
  f	�    � �  " e	� a   �      "  	    �    � �  " �� a   e	�   � _     � a     � �  " ��   � _     � a   �� �  " e	�   � _     � a     � �  "   
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
 �(�  L ( l       �        Dt    �� �   � P   �        Pt    �@    
� @  , 
�       \t    �� �   �p�               �L
�    %              � 8      ht    � $         �           
�    �      
"   
 �p� @  , 
�       xu    �� �   �p�               �L"    , 
"   
   p� @  , 
�       �u    �� n     p�               �L"    , 
"   
  p� @  , 
�       (v    �� I    p�               �L"    ,     %              %                   "      %                  "      �     "      �     "      4 (        "  
    �    � �  "   � a         "  	  e	�     "    f	T    "      "      @ A,    �   � _   �� �     "    �"       T      @   "    �(        "      � �    �� �      � _   �"    e	     "  	   %              D H   @ A,    �   � _   �� �     "    �"    f	,    S   "    �� �  " f	� a   �%                T      @   "    �(        "      � �    �� �      � _   �"    f	     "  
   %                         "    �� �     "    �           "      � �   �"      
�H T   %              �     }        �GG %              
"   
 f	
"   
   
"   
 f	
"   
 �(�  L ( l       �        Dz    �� �   � P   �        Pz    �@    
� @  , 
�       \z    �� �   f	p�               �L
�    %              � 8      hz    � $         �    �     
�    �    �
"   
 �p� @  , 
�       x{    �� n   �p�               �L"    , 
"   
   p� @  , 
�       �{    �� I     p�               �L"    , "      %               �     }        �%              %              %              %              %              %              %              %       	       %       
       %              %              %              %              %              %              %              %              "       "    �%     start-super-proc r�%     adm2/data.p %     start-super-proc r�%     adm2/dataext.p %     start-super-proc r�%     adm2/dataextcols.p 	%     start-super-proc r�%     adm2/dataextapi.p e	
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
 �(�  L ( l       �            �� �   � P   �            �@    
� @  , 
�       (    �� �   �p�               �L
�    %              � 8      4    � $         �    �     
�    �    �
"   
 �p� @  , 
�       D�    �� {   �p�               �L%               %     "Util/dalmcmov.i"   
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        �    �� �   � P   �        �    �@    
� @  , 
�       (�    �� �   �p�               �L
�    %              � 8      4�    � $         �           
�    �    �
"   
 �p� @  , 
�       D�    �� v   �p�               �L"    , 
�     	        �G
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        ��    �� �   � P   �        �    �@    
� @  , 
�       �    �� �   �p�               �L
�    %              � 8       �    � $         �           
�    �    �
"   
 �p� @  , 
�       0�    �� �  
 �p�               �L
"   
 , 
�     
        �G
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        �    �� �   � P   �        �    �@    
� @  , 
�        �    �� �   �p�               �L
�    %              � 8      �    � $         �           
�    �    �
"   
 �p� @  , 
�       �    �� {  	 �p�               �L
"   
 , 
"   
 �     � �  	   �        t�    �
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        �    �� �   � P   �         �    �@    
� @  , 
�       �    �� �   �p�               �L
�    %              � 8      �    � $         �           
�    �    �
"   
 �p� @  , 
�       (�    �� �   �p�               �L"    , 
"   
   �       ��    �"      
�     
        �G
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        �    �� �   � P   �        �    �@    
� @  , 
�       $�    �� �   �p�               �L
�    %              � 8      0�    � $         �           
�    �    �
"   
 �p� @  , 
�       @�    �� �  	 �p�               �L
"   
 , 
�             �Gp�,  8         $     
"   
 f	        �    �
�    
�             �Gp�,  8         $     
"   
 f	        �    �
�    �    � )     
�        "    f	� �    �%     modifyListProperty 
�    %      REMOVE  %     SupportedLinks %     Update-Target  %     bufferValidate  
�    "      �  %      setContextAndInitialize 
�    "      %     bufferCommit    
�    "      "      �    � �     
�    %               %     bufferCommit    
�    "      "      
�     
        �G�     }        �
�                    �           �   l       ��                 Y  }  �               lf	                    O   ����    e�          O   ����    R�          O   ����    ��        $  h  �   ���                       �U     
                    � ߱              i  (  �      V      4   ����V                �                      ��                  j  |                  $<f	                       j  8  �  �  k  PV            m  �  `      �V      4   �����V                p                      ��                  n  {                  �<f	                       n  �  �  o   o      ,                                 �  �   p  �V      �  �   q  �V      $  $  r  �  ���                        W     
                    � ߱        8  �   s  @W      L  �   t  `W      `  �   w  �W          $   z  �  ���                       �W  @         �W              � ߱                     T          ,  @   T �            
             
             
             
                 $   4   D          $   4   D   ����        ��                            ����                                            �           �   l       ��                 �  �  �               @>f	                    O   ����    e�          O   ����    R�          O   ����    ��      j                      �          �  $  �    ���                       X     
                    � ߱                  �  �                      ��                   �  �                  �Cf	                     �  4      4   ����$X      $  �  �  ���                       pX     
                    � ߱        �    �  4  D      �X      4   �����X      /  �  p                               3   �����X  �  �   �  �X          O   �  ��  ��  �X                               , �                          
                               �      ��                            ����                                            �           �   l       ��            	     H  �  �               ��e	                    O   ����    e�          O   ����    R�          O   ����    ��        $  h  �   ���                       `a                         � ߱        �  $  i  8  ���                       �a                         � ߱        �a     
                b  @         �a              � ߱        D  $   �  d  ���                         T      �  x                      ��        0         �  �                  �mf	      4c     L     �  �      $  �  �  ���                       �b                         � ߱          $  �  �  ���                       �b                         � ߱            4   ���� c  Tc     
                tc                     �c                         � ߱          $  �    ���                                                            ��                  �  �                  �qf	                �     �  �  �  $  �  L  ���                       pd       !       !           � ߱          �      L  �                      ��        0         �  �                  �uf	     ( �d            �  x      $  �     ���                       �d       (       (           � ߱        �  $  �  x  ���                       �d       (       (           � ߱            4   �����d        �  �  L      e      4   ����e                \                      ��                  �  �                  �xf	                       �  �  �  $  �  �  ���                       �e       !       !           � ߱            O   �  �� ��          $  �  �  ���                       �e                         � ߱        df     
                �f                     0h  @        
 �g          �h  @        
 Ph          �h                     �h     
                Xi                     �j  @        
 hj           k  @        
 �j          Xk  @        
 k              � ߱        x  V   �  $  ���                        P	    |  �  $	      dk      4   ����dk  �k                     �k                     �k                     Pl                         � ߱            $  }  �  ���                       �	    �  l	  |	      �l      4   �����l      �   �  �l      �	  $  �  �	  ���                       m                         � ߱        �
  $  �  
  ���                       $m                         � ߱          �
                              ��        0         �  �                  �{f	      �m     �     �  @
      $  �  �
  ���                       Dm                         � ߱        l  $  �  @  ���                       tm                         � ߱            4   �����m  �m                     n                     n                     ln                     �n                         � ߱        D  $  �  |  ���                             �  `  p      �n      4   �����n      $  �  �  ���                       �n           p             � ߱        �  $  �  �  ���                       p                         � ߱          �      �  \                      ��        0         �  �                  �|f	      �p          �         $  �  �  ���                        p                         � ߱        L  $  �     ���                       Pp                         � ߱            4   ����xp      $  �  �  ���                       �p                         � ߱        4q     
                �q                      s  @        
 �r              � ߱        �  V   �  �  ���                        s       
       
       @s       	       	       ts                     �s                         � ߱        �  $    D  ���                       �  $  �    ���                       �s                         � ߱        �s     
                tt                     �u  @        
 �u          v  @        
 �u          tv  @        
 4v              � ߱        |  V   �  H  ���                          �      �  \                      ��        0    	     &  ;                  �f	       w     4     &        $  &  �  ���                       �v                         � ߱        <  $  &    ���                       �v                         � ߱        L  4   �����v      4   ����w  �  $  +  �  ���                       xw                         � ߱        �    -  �  L      �w      4   �����w                �                      ��                  .  2                  ��f	                       .  �  �w                     Dx       	       	           � ߱            $  /  \  ���                             4  �  h      lx      4   ����lx  	              �                      ��             	     6  :                  �f	                       6  �   y                     hy       
       
           � ߱            $  7  x  ���                       �y                     �y                         � ߱        �  $  A  �  ���                       �y     
                tz                     �{  @        
 �{          |  @        
 �{              � ߱            V   O  `  ���                                    J `          �  �  � X@                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
             
                                                                                                                                                                                                                               ) �   �   �   �       (  8  H  X  h  x  �  �  �  �  �  �      (  8  H  X  h  x  �  �  �  �  �  �  �  �      (  8  H  8HXhx��������(8HX     ) �   �   �   �      (  8  H  X  h  x  �  �  �  �  �   �     (  8  H  X  h  x  �  �  �  �  �  �  �  �      (  8  H  8HXhx��������(8HX  �   :                  � �                     �    ��                      ��                            ����                            �                          �O                                �   l       ��                  �  	  �               ��f	                    O   ����    e�          O   ����    R�          O   ����    ��              !                    ��                            ����                                            �           �   l       ��                    "  �               ��f	                    O   ����    e�          O   ����    R�          O   ����    ��      e       �              �                  $                  d  /    $     4  �                      3   ������            T                      3   �����      O      ��  ��   �               �          �  �    �                                             ��                            ����                                            H          �   l       ��                  ,  W  �               �g	                    O   ����    e�          O   ����    R�          O   ����    ��      �       �              �                $                  �       ,             �          �                                �  /  K  t     �  H�                      3   ����$�            �                      3   ����P�     /  M  �     �  x�                      3   ����\�  x                             3   ������      $   M  L  ���                                                   � ߱                  �  �                  3   ������      $   M  �  ���                                                   � ߱        X  $  Q  ,  ���                       ��                         � ߱            O   U  ��  ��  ��               �          �  �   @ �                                                              0              0           ��                            ����                                                       �   l       ��                  a  �  �               �g	                    O   ����    e�          O   ����    R�          O   ����    ��      �       $                  �                    �          �                      �              /    L     \  �                      3   ����Ȍ  �        |  �                  3   �����      $     �  ���                                                   � ߱                                      3   ������      $     @  ���                                                   � ߱                     �          �  �   , �                                                                 ��                            ����                                            �           �   l       ��                  �  �  �               $��                    O   ����    e�          O   ����    R�          O   ����    ��            �  �   �       �      4   �����      �   �  ,�    ��                            ����                            TXS appSrvUtils Almcmov O:\on_in_co\Util\dalmcmov.w should only be RUN PERSISTENT. ADDROW CANCELROW CANNAVIGATE CLOSEQUERY COLUMNPROPS COLVALUES COPYROW CREATEROW DELETEROW FETCHROW FETCHROWIDENT FINDROW FINDROWWHERE FIRSTROWIDS GETLASTCOMMITERRORTYPE HASFOREIGNKEYCHANGED OPENDATAQUERY OPENQUERY PREPAREQUERY ROWAVAILABLE ROWVALUES SUBMITROW UPDATEROW GETOBJECTTYPE xiRocketIndexLimit ADDQUERYWHERE ASSIGNQUERYSELECTION BUFFERCOMPAREDBTORO BUFFERWHERECLAUSE COLUMNDATATYPE COLUMNDBCOLUMN COLUMNQUERYSELECTION COLUMNTABLE COLUMNVALMSG DBCOLUMNDATANAME DBCOLUMNHANDLE EXCLUDECOLUMNS GETDATACOLUMNS GETFOREIGNVALUES GETQUERYPOSITION GETQUERYSORT GETQUERYSTRING GETQUERYWHERE GETTARGETPROCEDURE INDEXINFORMATION INSERTEXPRESSION NEWQUERYSTRING NEWQUERYVALIDATE NEWQUERYWHERE NEWWHERECLAUSE REFRESHROWIDENT REMOVEFOREIGNKEY REMOVEQUERYSELECTION RESOLVEBUFFER ROWIDWHERE ROWIDWHERECOLS SETQUERYPOSITION SETQUERYSORT SETQUERYSTRING SETQUERYWHERE WHERECLAUSEBUFFER GETAPPSERVICE GETASBOUND GETASDIVISION GETASHANDLE GETASHASSTARTED GETASINFO GETASINITIALIZEONRUN GETASUSEPROMPT GETSERVERFILENAME GETSERVEROPERATINGMODE RUNSERVERPROCEDURE SETAPPSERVICE SETASDIVISION SETASHANDLE SETASINFO SETASINITIALIZEONRUN SETASUSEPROMPT SETSERVERFILENAME SETSERVEROPERATINGMODE gshAstraAppserver gshSessionManager gshRIManager gshSecurityManager gshProfileManager gshRepositoryManager gshTranslationManager gshWebManager gscSessionId gsdSessionObj gshFinManager gshGenManager gshAgnManager gsdTempUniqueID gsdUserObj gsdRenderTypeObj gsdSessionScopeObj ghProp ghADMProps ghADMPropsBuf glADMLoadFromRepos glADMOk ANYMESSAGE ASSIGNLINKPROPERTY FETCHMESSAGES GETCHILDDATAKEY GETCONTAINERHANDLE GETCONTAINERHIDDEN GETCONTAINERSOURCE GETCONTAINERSOURCEEVENTS GETCONTAINERTYPE GETDATALINKSENABLED GETDATASOURCE GETDATASOURCEEVENTS GETDATASOURCENAMES GETDATATARGET GETDATATARGETEVENTS GETDBAWARE GETDESIGNDATAOBJECT GETDYNAMICOBJECT GETINSTANCEPROPERTIES GETLOGICALOBJECTNAME GETLOGICALVERSION GETOBJECTHIDDEN GETOBJECTINITIALIZED GETOBJECTNAME GETOBJECTPAGE GETOBJECTPARENT GETOBJECTVERSION GETOBJECTVERSIONNUMBER GETPARENTDATAKEY GETPASSTHROUGHLINKS GETPHYSICALOBJECTNAME GETPHYSICALVERSION GETPROPERTYDIALOG GETQUERYOBJECT GETRUNATTRIBUTE GETSUPPORTEDLINKS GETTRANSLATABLEPROPERTIES GETUIBMODE GETUSERPROPERTY INSTANCEPROPERTYLIST LINKHANDLES LINKPROPERTY MAPPEDENTRY MESSAGENUMBER PROPERTYTYPE REVIEWMESSAGES SETCHILDDATAKEY SETCONTAINERHIDDEN SETCONTAINERSOURCE SETCONTAINERSOURCEEVENTS SETDATALINKSENABLED SETDATASOURCE SETDATASOURCEEVENTS SETDATASOURCENAMES SETDATATARGET SETDATATARGETEVENTS SETDBAWARE SETDESIGNDATAOBJECT SETDYNAMICOBJECT SETINSTANCEPROPERTIES SETLOGICALOBJECTNAME SETLOGICALVERSION SETOBJECTNAME SETOBJECTPARENT SETOBJECTVERSION SETPARENTDATAKEY SETPASSTHROUGHLINKS SETPHYSICALOBJECTNAME SETPHYSICALVERSION SETRUNATTRIBUTE SETSUPPORTEDLINKS SETTRANSLATABLEPROPERTIES SETUIBMODE SETUSERPROPERTY SHOWMESSAGE SIGNATURE prepareInstance ObjectName CHAR  ObjectVersion ADM2.2 ObjectType SmartDataObject ContainerType PropertyDialog adm2/support/datad.w QueryObject LOGICAL ContainerHandle HANDLE InstanceProperties AppService,ASInfo,ASUsePrompt,CacheDuration,CheckCurrentChanged,DestroyStateless,DisconnectAppServer,ServerOperatingMode,ShareData,UpdateFromSource,ForeignFields,ObjectName,OpenOnInit,PromptColumns,PromptOnDelete,RowsToBatch,RebuildOnRepos,ToggleDataTargets SupportedLinks Data-Source,Data-Target,Navigation-Target,Update-Target,Commit-Target,Filter-Target ContainerHidden ObjectInitialized ObjectHidden ObjectsCreated HideOnInit UIBMode ContainerSource ContainerSourceEvents initializeObject,hideObject,viewObject,destroyObject,enableObject,confirmExit,confirmCancel,confirmOk,isUpdateActive DataSource DataSourceEvents dataAvailable,queryPosition,updateState,deleteComplete,fetchDataSet,confirmContinue,confirmCommit,confirmUndo,assignMaxGuess,isUpdatePending TranslatableProperties ObjectPage INT DBAware DesignDataObject DataSourceNames DataTarget DataTargetEvents CHARACTER updateState,rowObjectState,fetchBatch,LinkState LogicalObjectName PhysicalObjectName LogicalVersion PhysicalVersion DynamicObject RunAttribute ChildDataKey ParentDataKey DataLinksEnabled InactiveLinks InstanceId DECIMAL SuperProcedure SuperProcedureMode SuperProcedureHandle LayoutPosition ClassName RenderingProcedure ThinRenderingProcedure Label cType ADMProps Target WHERE Target = WIDGET-H(" ") AppService ASDivision ASHandle ASHasConnected ASHasStarted ASInfo ASInitializeOnRun ASUsePrompt BindSignature BindScope ServerOperatingMode ServerFileName ServerFirstCall NeedContext AutoCommit BLOBColumns BufferHandles CLOBColumns CommitSource CommitSourceEvents commitTransaction,undoTransaction CommitTarget CommitTargetEvents rowObjectState CurrentRowid ROWID CurrentUpdateSource DataColumns DataHandle DataIsFetched DataModified DataQueryBrowsed DataQueryString FetchOnOpen FillBatchOnRepos FilterActive FilterAvailable FilterSource FilterWindow FirstRowNum ForeignFields ForeignValues IgnoreTreeViewFilter IndexInformation LargeColumns LastRowNum NavigationSource NavigationSourceEvents fetchFirst,fetchNext,fetchPrev,fetchLast,startFilter OpenOnInit PrimarySDOSource PromptColumns PromptOnDelete QueryColumns QueryPosition QueryString RebuildOnRepos RowObject RowObjectState NoUpdates RowsToBatch Tables ToggleDataTargets TransferChildrenForAll UpdatableColumns UpdatableWhenNew UpdateSource AssignList AuditEnabled BaseQuery CalcFieldList CheckLastOnOpen DataColumnsByTable DBNames EntityFields FetchHasAudit FetchHasComment FetchAutoComment FirstResultRow KeyFields KeyTableId LastDBRowIdent LastResultRow NewBatchInfo NoLockReadOnlyTables PhysicalTables PositionForClient QueryHandle QueryRowIdent RequiredProperties SkipTransferDBRow TempTables UpdatableColumnsByTable UpdateFromSource RowObjUpd RowObjectTable RowObjUpdTable CheckCurrentChanged StatelessSavedProperties CheckCurrentChanged,RowObjectState,LastResultRow,FirstResultRow,QueryRowIdent DestroyStateless DisconnectAppServer ServerSubmitValidation DataFieldDefs "Util/dalmcmov.i" QueryContainer QueryContext AsynchronousSDO DataLogicProcedure DataLogicObject DataReadHandler DataReadColumns DataReadBuffer DataDelimiter | DataReadFormat TrimNumeric IsRowObjectExternal IsRowObjUpdExternal ManualAddQueryWhere DynamicData LastCommitErrorType LastCommitErrorKeys RunDataLogicProxy SchemaLocation CacheDuration INTEGER ShareData ghContainer adm2/smart.p cObjectName iStart / \ . hReposBuffer hPropTable hBuffer hTable deleteProperties ADM-CLONE-PROPS pcProcName hProc START-SUPER-PROC cAppService cASDivision cServerOperatingMode adm2/appserver.p getAppService Server NONE setASDivision setAppService adm2/dataquery.p dataAvailable,confirmContinue,isUpdatePending,buildDataRequest adm2/query.p adm2/queryext.p cTable iTable cColumns cDataCols cUpdCols cCalcData cCalcUpd iNumData iNumUpd cBuffers cKeyFields cAssignList iAssigns iPos iEntry iCount cTables cTableAssign cDbEntry cField cKeyTable cQueryString FOR EACH Almcmov NO-LOCK INDEXED-REPOSITION ,   hQuery cOpenQuery cDBNames cPhysicalTables cKeyTableEntityFields cKeyTableEntityValues cKeyTableEntityMnemonic cKeyTableEntityObjField cDBName cEntityFields lHasObjectField lHasAudit lHasComment lHasAutoComment iLookup iAlias  STATIC setDBNames OPEN QUERY Query-Main FOR EACH Almcmov NO-LOCK INDEXED-REPOSITION.  FOR   PRESELECT  setOpenQuery 5 showMessage ; TipMov CodMov NroSer NroDoc FchDoc Query-Main INITPROPS piTableIndex lRet DELETERECORDSTATIC adm2/data.p adm2/dataext.p adm2/dataextcols.p adm2/dataextapi.p TipMov CodMov NroSer NroDoc FchDoc RowNum RowIdent RowMod RowIdentIdx RowUserProp ChangedFields cContainerType hRowObject hDataQuery cDataFieldDefs FOR EACH  setRowObjectTable setRowObjUpdTable getUpdatableColumns REMOVE Update-Target PUSHROWOBJUPDTABLE pcValType PUSHTABLEANDVALIDATE pcContext pcMessages pcUndoIds obtainContextForClient REMOTECOMMIT SERVERCOMMIT GETROWOBJUPDSTATIC DISABLE_UI qDataQuery   <0  @  t>      / �<   ��      0         pcServices      ��      T         pcServices  �   ��      x         piStartRow  �   ��      �         piStartRow  �   ��      �         piStartRow  �   ��      �         piStartRow      ��              piStartRow  <  ��      ,        pcViewColList       ��      T       
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
 hTarget �  ��      �        pcMessage       ��      �        pcMessage       ��      �        plEnabled             �     cType       0     V   �                             getObjectType   K	  c	  e	  `        P  
   hReposBuffer    �        t  
   hPropTable  �        �  
   hBuffer           �  
   hTable  �  �     W   <          �                  adm-clone-props h  i  j  k  m  n  o  p  q  r  s  t  w  z  {  |  }            P  
   hProc             p        pcProcName  �  �  	   X   <  X      �                  start-super-proc    �  �  �  �  �  �  �  �  �           �     cTable               iTable  <        0     cColumns    \        P     cDataCols   |        p     cUpdCols    �        �     cCalcData   �        �     cCalcUpd    �     	   �     iNumData    �     
   �     iNumUpd              cBuffers    8        ,     cKeyFields  X        L     cAssignList x        l     iAssigns    �        �     iPos    �        �     iEntry  �        �     iCount  �        �     cTables        �     cTableAssign    ,              cDbEntry    H        @     cField  h        \     cKeyTable   �        |     cQueryString    �        �  
   hQuery  �        �  
   hBuffer �        �     cOpenQuery          �     cDBNames    (             cPhysicalTables T        <     cKeyTableEntityFields   �        h     cKeyTableEntityValues   �        �     cKeyTableEntityMnemonic �         �     cKeyTableEntityObjField �     !   �     cDBName      "        cEntityFields   <     #   ,     lHasObjectField \     $   P     lHasAudit   |     %   p     lHasComment �     &   �     lHasAutoComment �     '   �     iLookup        (   �     iAlias  |    3   Y   �                            initProps   h  i  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  |  }  �  �  �  �  �  �  �  �  �  �  �  �  �  �    �  �  &  +  -  .  /  2  4  6  7  :  ;  A  O  �            �     lRet                      piTableIndex    �  h  *   Z   �  �      T                  deleteRecordStatic  �  �  �  �  �  �  �  �           :  ;  W  X  t  u  �  �  �  �  �  �  �  �      "  #  ?  @  \  ]  y  z  �  �  �  �  �  �                 !       $  l     [             X                  pushRowObjUpdTable  	  �        �        pcValType                  $       (  �     \       p      �                  pushTableAndValidate         "  $                pcContext   <             $       `        T        pcMessages            x        pcUndoIds   �  �     ]              �                  remoteCommit    K  M  Q  U  W  �             $                       pcMessages            ,        pcUndoIds   �  x     ^       �      h                  serverCommit      �  8  �     _               �                  getRowObjUpdStatic  �  �  �       `               �                  disable_UI  �  �  �  �!  
     <      D!                      0  P  \  
   RowObject   �         �         �         �         �         �                                    $         TipMov  CodMov  NroSer  NroDoc  FchDoc  RowNum  RowIdent    RowMod  RowIdentIdx RowUserProp     @  L     RowObjUpd   �         �         �         �         �         �                                               ,         TipMov  CodMov  NroSer  NroDoc  FchDoc  RowNum  RowIdent    RowMod  RowIdentIdx RowUserProp ChangedFields   \          P  
   appSrvUtils �       p     xiRocketIndexLimit  �        �  
   gshAstraAppserver   �        �  
   gshSessionManager   �        �  
   gshRIManager               
   gshSecurityManager  H        4  
   gshProfileManager   t  	 	     \  
   gshRepositoryManager    �  
 
     �  
   gshTranslationManager   �        �  
   gshWebManager   �        �     gscSessionId            �     gsdSessionObj   0           
   gshFinManager   T        D  
   gshGenManager   x        h  
   gshAgnManager   �        �     gsdTempUniqueID �        �     gsdUserObj  �        �     gsdRenderTypeObj            �     gsdSessionScopeObj  (          
   ghProp  H       <  
   ghADMProps  l       \  
   ghADMPropsBuf   �       �     glADMLoadFromRepos  �       �     glADMOk �       �  
   ghContainer �    	   �     cObjectName      
         iStart  ,               cAppService L        @      cASDivision x        `      cServerOperatingMode    �        �      cContainerType  �        �      cQueryString    �        �   
   hRowObject   !       �   
   hDataQuery   !       !     cColumns             4!     cDataFieldDefs  \!       T!  Almcmov x!    X  l!  RowObject         X  �!  RowObjUpd            9   �   �   �   �           3  ?  @  A  C  E  F  G  K  L  O  P  Q  R  T  V  X  Z  [  \  _  a  b  d  e  f  g  h  n  p  v  x  z  {  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  
  <
  =
  ?
  @
  A
  B
  C
  D
  F
  G
  H
  I
  J
  K
  L
  M
  N
  O
  P
  Q
  R
  S
  T
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
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
                     y  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  (  �  �  �  �  �  �  �  �  �  �    +  J  L  a  �        ,  -  .  1  2  3  :  ;  X  l  �      +  O  �  �  �  �  �  <  �  �  �  �  �  �  �  0  J  T  n  x  �  �  �  �  �  �  �       $  >  `  k  l      ��  C:\Progress\OpenEdge\src\adm2\data.i �%  �) . %C:\Progress\OpenEdge\src\adm2\custom\datacustom.i    �%  �� - C:\Progress\OpenEdge\src\adm2\robjflds.i ,&  �� , O:\on_in_co\Util\dalmcmov.i  `&  �:  C:\Progress\OpenEdge\src\adm2\query.i    �&  z + C:\Progress\OpenEdge\src\adm2\delrecst.i �&  `W * C:\Progress\OpenEdge\src\adm2\tblprep.i  �&  F� ) C:\Progress\OpenEdge\gui\fnarg   $'   ( %C:\Progress\OpenEdge\src\adm2\custom\querycustom.i   P'  �   C:\Progress\OpenEdge\src\adm2\dataquery.i    �'  �Z ' %C:\Progress\OpenEdge\src\adm2\custom\dataquerycustom.i   �'  �< ! C:\Progress\OpenEdge\src\adm2\appserver.i    (  �� & %C:\Progress\OpenEdge\src\adm2\custom\appservercustom.i   D(  I� " C:\Progress\OpenEdge\src\adm2\smart.i    �(  Ds % C:\Progress\OpenEdge\gui\fn  �(  tw $ %C:\Progress\OpenEdge\src\adm2\custom\smartcustom.i   �(  Q. # C:\Progress\OpenEdge\gui\set $)  �>  C:\Progress\OpenEdge\src\adm2\dataprop.i L)  ��  %C:\Progress\OpenEdge\src\adm2\custom\datapropcustom.i    �)  ��  %C:\Progress\OpenEdge\src\adm2\custom\dataprtocustom.i    �)  �K  C:\Progress\OpenEdge\src\adm2\qryprop.i  *  -�  %C:\Progress\OpenEdge\src\adm2\custom\qrypropcustom.i <*  ��  %C:\Progress\OpenEdge\src\adm2\custom\qryprtocustom.i |*   	 C:\Progress\OpenEdge\src\adm2\dataqueryprop.i    �*  �d  %C:\Progress\OpenEdge\src\adm2\custom\dataquerypropcustom.i   �*  ��  %C:\Progress\OpenEdge\src\adm2\custom\dataqueryprtocustom.i   @+  �l  C:\Progress\OpenEdge\src\adm2\appsprop.i �+  ɏ  %C:\Progress\OpenEdge\src\adm2\custom\appspropcustom.i    �+  V  %C:\Progress\OpenEdge\src\adm2\custom\appsprtocustom.i     ,  i$  C:\Progress\OpenEdge\src\adm2\smrtprop.i D,  �j  C:\Progress\OpenEdge\gui\get x,  �  %C:\Progress\OpenEdge\src\adm2\custom\smrtpropcustom.i    �,  ��  %C:\Progress\OpenEdge\src\adm2\custom\smrtprtocustom.i    �,  ��  C:\Progress\OpenEdge\src\adm2\smrtprto.i (-  Su  C:\Progress\OpenEdge\src\adm2\globals.i  \-  M�  %C:\Progress\OpenEdge\src\adm2\custom\globalscustom.i �-  )a  %C:\Progress\OpenEdge\src\adm2\custom\smartdefscustom.i   �-  �  C:\Progress\OpenEdge\src\adm2\appsprto.i .  ��  %C:\Progress\OpenEdge\src\adm2\custom\appserverdefscustom.i   H.  ��  C:\Progress\OpenEdge\src\adm2\dataqueryprto.i    �.  ª 
 %C:\Progress\OpenEdge\src\adm2\custom\dataquerydefscustom.i   �.  ��  C:\Progress\OpenEdge\src\adm2\qryprto.i  /  �  %C:\Progress\OpenEdge\src\adm2\custom\querydefscustom.i   H/  �`  C:\Progress\OpenEdge\src\adm2\dataprto.i �/  �  %C:\Progress\OpenEdge\src\adm2\custom\datadefscustom.i    �/  e�  !C:\Progress\OpenEdge\gui\adecomm\appserv.i   0  <d    O:\on_in_co\Util\dalmcmov.w      �   �      d0  [  T     t0     R  %   �0  �   �     �0     t  .   �0  �   j     �0     K     �0  �   H     �0     &  #   �0  �   $     �0       #   1  �         1     �  #   $1  �   �     41     �  #   D1  �   �     T1     �  #   d1  �   �     t1     p  #   �1  �   n     �1     L  #   �1  �   J     �1     (  #   �1  �        �1       -   �1  �   �     �1     �  ,   2  k   �     2  �  �     $2     �  +   42  �  �     D2     |  +   T2  �  y     d2     _  +   t2  �  \     �2     B  +   �2  �  ?     �2     %  +   �2  �  "     �2       +   �2  �       �2     �  +   �2  �  �     3     �  +   3  �  �     $3     �  +   43  �  �     D3     �  +   T3  �  �     d3     w  +   t3  �  t     �3     Z  +   �3  �  W     �3     =  +   �3  �  :     �3        +   �3  �       �3       +   �3  �        4     �  +   4  �  �     $4     �  +   44  �  �     D4     �  +   T4  �  �     d4     j  #   t4  �  i     �4     G  #   �4  k  "     �4        #   �4  j  �     �4     �  #   �4  i  �     �4     �  #   �4  _  �     5     �  *   5  ^  �     $5     c  *   45  ]  b     D5     <  *   T5  \  ;     d5       *   t5  [       �5     �  *   �5  Z  �     �5     �  *   �5  Y  �     �5     �  *   �5  X  �     �5     y  *   �5  W  x     6     R  *   6  V  Q     $6     +  *   46  U  *     D6       *   T6  T       d6     �  *   t6  S  �     �6     �  *   �6  R  �     �6     �  *   �6  Q  �     �6     h  *   �6  P  g     �6     A  *   �6  O  @     7       *   7  N       $7     �  *   47  @  �     D7     �  #   T7  	  �     d7     �  )   t7  �   y     �7     W  #   �7  �   V     �7     4  #   �7  �   3     �7       #   �7  �        �7     �  #   �7  �   �     8     �  #   8  �   �     $8     �  #   48  �   8     D8     �  (   T8  g   �     d8  a   �      t8     k  '   �8  _   i      �8     G  #   �8  ]   E      �8     #  #   �8  I         �8  �     !   �8     �  &   �8  �   �  !   9     �  #   9  �   �  !   $9     d  #   49  �   b  !   D9     @  #   T9  g   &  !   d9          t9  O   �  !   �9  �   y  "   �9     w  %   �9  �   G  "   �9     �  $   �9  �   �  "   �9     �  #   �9  �   �  "   �9     �  #   :  �   �  "   :     |  #   $:  �   {  "   4:     Y  #   D:  �   E  "   T:     #  #   d:  }     "   t:     �  #   �:     y  "   �:     +  !   �:     �      �:     }     �:     1     �:  �   (     �:  O        �:     	     ;     �     ;  �   �     $;  �   y     4;  O   k     D;     Z     T;          d;  y   �
     t;  �   �
  	   �;  G   �
     �;     �
     �;     t
     �;  c   
  	   �;  x   
     �;  M   �	     �;     �	     �;     �	     <  a   �	     <  �  b	     $<     C	     4<  �  	     D<  O   	     T<     �     d<     �     t<  �   �     �<     �     �<     �     �<  x   �     �<     �     �<     ^     �<     Z     �<     F     �<     -     =  Q        =     �     $=     �     4=     w     D=     ]     T=  ]   W  	   d=     M     t=       	   �=     �  
   �=     �  	   �=  Z   �     �=     �     �=     �     �=     �     �=     �     �=  c   a     >     ?     >     �      $>     �      4>     �      D>     �      T>     !       d>           