	��V�I�6   �                                              P 36E000EFutf-8 MAIN E:\OpenEdge\on_in_co\APLIC\q-almcen_cl.w,, PROCEDURE disable_UI,, PROCEDURE serverCommit,,INPUT-OUTPUT RowObjUpd TABLE,OUTPUT pcMessages CHARACTER,OUTPUT pcUndoIds CHARACTER PROCEDURE remoteCommit,,INPUT-OUTPUT pcContext CHARACTER,INPUT-OUTPUT RowObjUpd TABLE,OUTPUT pcMessages CHARACTER,OUTPUT pcUndoIds CHARACTER PROCEDURE pushTableAndValidate,,INPUT pcValType CHARACTER,INPUT-OUTPUT RowObjUpd TABLE PROCEDURE pushRowObjUpdTable,,INPUT RowObjUpd TABLE PROCEDURE initProps,, PROCEDURE start-super-proc,,INPUT pcProcName CHARACTER PROCEDURE adm-clone-props,, PROCEDURE viewObject,, PROCEDURE toggleData,,INPUT plEnabled LOGICAL PROCEDURE showMessageProcedure,,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL PROCEDURE returnFocus,,INPUT hTarget HANDLE PROCEDURE repositionObject,,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL PROCEDURE removeLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE removeAllLinks,, PROCEDURE modifyUserLinks,,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE PROCEDURE modifyListProperty,,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER PROCEDURE hideObject,, PROCEDURE exitObject,, PROCEDURE editInstanceProperties,, PROCEDURE displayLinks,, PROCEDURE createControls,, PROCEDURE changeCursor,,INPUT pcCursor CHARACTER PROCEDURE applyEntry,,INPUT pcField CHARACTER PROCEDURE adjustTabOrder,,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER PROCEDURE addMessage,,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER PROCEDURE addLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE unbindServer,,INPUT pcMode CHARACTER PROCEDURE runServerObject,,INPUT phAppService HANDLE PROCEDURE disconnectObject,, PROCEDURE destroyObject,, PROCEDURE bindServer,, PROCEDURE transferDBRow,,INPUT pcRowIdent CHARACTER,INPUT piRowNum INTEGER PROCEDURE startFilter,, PROCEDURE releaseDBRow,, PROCEDURE refetchDBRow,,INPUT phRowObjUpd HANDLE PROCEDURE filterContainerHandler,,INPUT phFilterContainer HANDLE PROCEDURE fetchDBRowForUpdate,, PROCEDURE confirmContinue,,INPUT-OUTPUT pioCancel LOGICAL PROCEDURE compareDBRow,, PROCEDURE bufferCopyDBToRO,,INPUT phRowObj HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER PROCEDURE assignDBRow,,INPUT phRowObjUpd HANDLE PROCEDURE updateState,,INPUT pcState CHARACTER PROCEDURE updateQueryPosition,, PROCEDURE updateAddQueryWhere,,INPUT pcWhere CHARACTER,INPUT pcField CHARACTER PROCEDURE undoTransaction,, PROCEDURE transferToExcel,,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT plUseExisting LOGICAL,INPUT piMaxRecords INTEGER PROCEDURE synchronizeProperties,,INPUT pcPropertiesForServer CHARACTER,OUTPUT pcPropertiesForClient CHARACTER PROCEDURE submitValidation,,INPUT pcValueList CHARACTER,INPUT pcUpdColumns CHARACTER PROCEDURE submitForeignKey,,INPUT pcRowIdent CHARACTER,INPUT-OUTPUT pcValueList CHARACTER,INPUT-OUTPUT pcUpdColumns CHARACTER PROCEDURE submitCommit,,INPUT pcRowIdent CHARACTER,INPUT plReopen LOGICAL PROCEDURE startServerObject,, PROCEDURE setPropertyList,,INPUT pcProperties CHARACTER PROCEDURE serverFetchRowObjUpdTable,,OUTPUT phRowObjUpd TABLE-HANDLE PROCEDURE serverSendRows,,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER,OUTPUT phRowObject TABLE-HANDLE PROCEDURE saveContextAndDestroy,,OUTPUT pcContext CHARACTER PROCEDURE rowObjectState,,INPUT pcState CHARACTER PROCEDURE retrieveFilter,, PROCEDURE restartServerObject,, PROCEDURE remoteSendRows,,INPUT-OUTPUT piocContext CHARACTER,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT pioRowsReturned INTEGER,OUTPUT phRowObject TABLE-HANDLE,OUTPUT pocMessages CHARACTER PROCEDURE refreshRow,, PROCEDURE printToCrystal,,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT piMaxRecords INTEGER PROCEDURE isUpdatePending,,INPUT-OUTPUT plUpdate LOGICAL PROCEDURE initializeServerObject,, PROCEDURE initializeObject,, PROCEDURE home,, PROCEDURE genContextList,,OUTPUT pcContext CHARACTER PROCEDURE fetchPrev,, PROCEDURE fetchNext,, PROCEDURE fetchLast,, PROCEDURE fetchFirst,, PROCEDURE fetchBatch,,INPUT plForwards LOGICAL PROCEDURE endClientDataRequest,, PROCEDURE destroyServerObject,, PROCEDURE describeSchema,,INPUT pcSdoName CHARACTER,OUTPUT hTtSchema TABLE-HANDLE PROCEDURE dataAvailable,,INPUT pcRelative CHARACTER PROCEDURE copyColumns,,INPUT pcViewColList CHARACTER,INPUT phDataQuery HANDLE PROCEDURE commitTransaction,, PROCEDURE clientSendRows,,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER PROCEDURE batchServices,,INPUT pcServices CHARACTER,OUTPUT pcValues CHARACTER FUNCTION deleteRecordStatic,logical,INPUT piTableIndex INTEGER FUNCTION getRowObjUpdStatic,widget-handle, FUNCTION Signature,CHARACTER,INPUT pcName CHARACTER FUNCTION showmessage,LOGICAL,INPUT pcMessage CHARACTER FUNCTION setUserProperty,LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION setUIBMode,LOGICAL,INPUT pcMode CHARACTER FUNCTION setTranslatableProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setSupportedLinks,LOGICAL,INPUT pcLinkList CHARACTER FUNCTION setRunAttribute,LOGICAL,INPUT cRunAttribute CHARACTER FUNCTION setPhysicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setPhysicalObjectName,LOGICAL,INPUT cTemp CHARACTER FUNCTION setPassThroughLinks,LOGICAL,INPUT pcLinks CHARACTER FUNCTION setParentDataKey,LOGICAL,INPUT cParentDataKey CHARACTER FUNCTION setObjectVersion,LOGICAL,INPUT cObjectVersion CHARACTER FUNCTION setObjectParent,LOGICAL,INPUT phParent HANDLE FUNCTION setObjectName,LOGICAL,INPUT pcName CHARACTER FUNCTION setLogicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setLogicalObjectName,LOGICAL,INPUT c CHARACTER FUNCTION setInstanceProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setDynamicObject,LOGICAL,INPUT lTemp LOGICAL FUNCTION setDesignDataObject,LOGICAL,INPUT pcDataObject CHARACTER FUNCTION setDBAware,LOGICAL,INPUT lAware LOGICAL FUNCTION setDataTargetEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setDataTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setDataSourceNames,LOGICAL,INPUT pcSourceNames CHARACTER FUNCTION setDataSourceEvents,LOGICAL,INPUT pcEventsList CHARACTER FUNCTION setDataSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDataLinksEnabled,LOGICAL,INPUT lDataLinksEnabled LOGICAL FUNCTION setContainerSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setContainerSource,LOGICAL,INPUT phObject HANDLE FUNCTION setContainerHidden,LOGICAL,INPUT plHidden LOGICAL FUNCTION setChildDataKey,LOGICAL,INPUT cChildDataKey CHARACTER FUNCTION reviewMessages,CHARACTER, FUNCTION propertyType,CHARACTER,INPUT pcPropName CHARACTER FUNCTION messageNumber,CHARACTER,INPUT piMessage INTEGER FUNCTION mappedEntry,CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER FUNCTION linkProperty,CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER FUNCTION linkHandles,CHARACTER,INPUT pcLink CHARACTER FUNCTION instancePropertyList,CHARACTER,INPUT pcPropList CHARACTER FUNCTION getUserProperty,CHARACTER,INPUT pcPropName CHARACTER FUNCTION getUIBMode,CHARACTER, FUNCTION getTranslatableProperties,CHARACTER, FUNCTION getSupportedLinks,CHARACTER, FUNCTION getRunAttribute,CHARACTER, FUNCTION getQueryObject,LOGICAL, FUNCTION getPropertyDialog,CHARACTER, FUNCTION getPhysicalVersion,CHARACTER, FUNCTION getPhysicalObjectName,CHARACTER, FUNCTION getPassThroughLinks,CHARACTER, FUNCTION getParentDataKey,CHARACTER, FUNCTION getObjectVersionNumber,CHARACTER, FUNCTION getObjectVersion,CHARACTER, FUNCTION getObjectParent,HANDLE, FUNCTION getObjectPage,INTEGER, FUNCTION getObjectName,CHARACTER, FUNCTION getObjectInitialized,LOGICAL, FUNCTION getObjectHidden,LOGICAL, FUNCTION getLogicalVersion,CHARACTER, FUNCTION getLogicalObjectName,CHARACTER, FUNCTION getInstanceProperties,CHARACTER, FUNCTION getDynamicObject,LOGICAL, FUNCTION getDesignDataObject,CHARACTER, FUNCTION getDBAware,LOGICAL, FUNCTION getDataTargetEvents,CHARACTER, FUNCTION getDataTarget,CHARACTER, FUNCTION getDataSourceNames,CHARACTER, FUNCTION getDataSourceEvents,CHARACTER, FUNCTION getDataSource,HANDLE, FUNCTION getDataLinksEnabled,LOGICAL, FUNCTION getContainerType,CHARACTER, FUNCTION getContainerSourceEvents,CHARACTER, FUNCTION getContainerSource,HANDLE, FUNCTION getContainerHidden,LOGICAL, FUNCTION getContainerHandle,HANDLE, FUNCTION getChildDataKey,CHARACTER, FUNCTION fetchMessages,CHARACTER, FUNCTION assignLinkProperty,LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION anyMessage,LOGICAL, FUNCTION setServerOperatingMode,LOGICAL,INPUT pcServerOperatingMode CHARACTER FUNCTION setServerFileName,LOGICAL,INPUT pcFileName CHARACTER FUNCTION setASUsePrompt,LOGICAL,INPUT plFlag LOGICAL FUNCTION setASInitializeOnRun,LOGICAL,INPUT plInitialize LOGICAL FUNCTION setASInfo,LOGICAL,INPUT pcInfo CHARACTER FUNCTION setASHandle,LOGICAL,INPUT phASHandle HANDLE FUNCTION setASDivision,LOGICAL,INPUT pcDivision CHARACTER FUNCTION setAppService,LOGICAL,INPUT pcAppService CHARACTER FUNCTION runServerProcedure,HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE FUNCTION getServerOperatingMode,CHARACTER, FUNCTION getServerFileName,CHARACTER, FUNCTION getASUsePrompt,LOGICAL, FUNCTION getASInitializeOnRun,LOGICAL, FUNCTION getASInfo,CHARACTER, FUNCTION getASHasStarted,LOGICAL, FUNCTION getASHandle,HANDLE, FUNCTION getAsDivision,CHARACTER, FUNCTION getASBound,LOGICAL, FUNCTION getAppService,CHARACTER, FUNCTION whereClauseBuffer,CHARACTER,INPUT pcWhere CHARACTER FUNCTION setQueryWhere,LOGICAL,INPUT pcWhere CHARACTER FUNCTION setQueryString,LOGICAL,INPUT pcQueryString CHARACTER FUNCTION setQuerySort,LOGICAL,INPUT pcSort CHARACTER FUNCTION setQueryPosition,LOGICAL,INPUT pcPosition CHARACTER FUNCTION rowidWhereCols,CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER FUNCTION rowidWhere,CHARACTER,INPUT pcWhere CHARACTER FUNCTION resolveBuffer,CHARACTER,INPUT pcBuffer CHARACTER FUNCTION removeQuerySelection,LOGICAL,INPUT pcColumns CHARACTER,INPUT pcOperators CHARACTER FUNCTION removeForeignKey,LOGICAL, FUNCTION refreshRowident,CHARACTER,INPUT pcRowident CHARACTER FUNCTION newWhereClause,CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcExpression CHARACTER,INPUT pcWhere CHARACTER,INPUT pcAndOr CHARACTER FUNCTION newQueryWhere,CHARACTER,INPUT pcWhere CHARACTER FUNCTION newQueryValidate,CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcExpression CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER FUNCTION newQueryString,CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcAndOr CHARACTER FUNCTION insertExpression,CHARACTER,INPUT pcWhere CHARACTER,INPUT pcExpression CHARACTER,INPUT pcAndOr CHARACTER FUNCTION indexInformation,CHARACTER,INPUT pcQuery CHARACTER,INPUT plUseTableSep LOGICAL,INPUT pcIndexInfo CHARACTER FUNCTION getTargetProcedure,HANDLE, FUNCTION getQueryWhere,CHARACTER, FUNCTION getQueryString,CHARACTER, FUNCTION getQuerySort,CHARACTER, FUNCTION getQueryPosition,CHARACTER, FUNCTION getForeignValues,CHARACTER, FUNCTION getDataColumns,CHARACTER, FUNCTION excludeColumns,CHARACTER,INPUT iTable INTEGER FUNCTION dbColumnHandle,HANDLE,INPUT pcColumn CHARACTER FUNCTION dbColumnDataName,CHARACTER,INPUT pcDbColumn CHARACTER FUNCTION columnValMsg,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnTable,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnQuerySelection,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnDbColumn,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnDataType,CHARACTER,INPUT pcColumn CHARACTER FUNCTION bufferWhereClause,CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcWhere CHARACTER FUNCTION bufferCompareDBToRO,LOGICAL,INPUT phRowObjUpd HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER FUNCTION assignQuerySelection,LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER FUNCTION addQueryWhere,LOGICAL,INPUT pcWhere CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER FUNCTION getObjectType,character, FUNCTION updateRow,LOGICAL,INPUT pcKeyValues CHARACTER,INPUT pcValueList CHARACTER FUNCTION submitRow,LOGICAL,INPUT pcRowIdent CHARACTER,INPUT pcValueList CHARACTER FUNCTION rowValues,CHARACTER,INPUT pcColumns CHARACTER,INPUT pcFormat CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION rowAvailable,LOGICAL,INPUT pcDirection CHARACTER FUNCTION prepareQuery,LOGICAL,INPUT pcQuery CHARACTER FUNCTION openQuery,LOGICAL, FUNCTION openDataQuery,LOGICAL,INPUT pcPosition CHARACTER FUNCTION hasForeignKeyChanged,LOGICAL, FUNCTION getLastCommitErrorType,CHARACTER, FUNCTION firstRowIds,CHARACTER,INPUT pcQueryString CHARACTER FUNCTION findRowWhere,LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER FUNCTION findRow,LOGICAL,INPUT pcKeyValues CHARACTER FUNCTION fetchRowIdent,CHARACTER,INPUT pcRowIdent CHARACTER,INPUT pcViewColList CHARACTER FUNCTION fetchRow,CHARACTER,INPUT piRow INTEGER,INPUT pcViewColList CHARACTER FUNCTION deleteRow,LOGICAL,INPUT pcRowIdent CHARACTER FUNCTION createRow,LOGICAL,INPUT pcValueList CHARACTER FUNCTION copyRow,CHARACTER,INPUT pcViewColList CHARACTER FUNCTION colValues,CHARACTER,INPUT pcViewColList CHARACTER FUNCTION columnProps,CHARACTER,INPUT pcColList CHARACTER,INPUT pcPropList CHARACTER FUNCTION closeQuery,LOGICAL, FUNCTION canNavigate,LOGICAL, FUNCTION cancelRow,CHARACTER, FUNCTION addRow,CHARACTER,INPUT pcViewColList CHARACTER TEMP-TABLE RowObjUpd 0,RowNum:RowNum 0 NO,CodCia integer 0 0,CodAlm character 1 0,Descripcion character 2 0,RowNum integer 3 0,RowIdent character 4 0,RowMod character 5 0,RowIdentIdx character 6 0,RowUserProp character 7 0,ChangedFields character 8 0       \              p�              D� \  �              ��              ;     +   � t  W   �� D  X   ̊ |  Y   H�   [   L�   \   X� 0  ]   ��   ^   ��    `   ? �� �  iSO8859-1                                                                           �    �                                      �                   ��                   �       �s   8�              ��  �   $      0                                                         PROGRESS                         �           
    
                    �              �                                                                                                     
  H  �      �  
    
                  �  x             4                                                                                          �          
  �  �      p  
    
                  \  $             �                                                                                          �          
  �          
    
                    �             �                                                                                                    
  L        �  
    
                  �  |             8                                                                                                    
  �  0      t  
    
                  `  (             �                                                                                          0          
  �  B         
    
                    �             �                                                                                          B          
  P  W      �  
    
                  �  �  	           <                                                                                          W          
  �  m      x  
    
                  d  ,  
           �                                                                                          m          
  �  {      $                           �             �                                                                                          {            T  �      �                        �  �             @                                                                                          �             	  �      |  
    
                  h  0	             �                                                                                          �          
  �	  �      (	  
    
                  	  �	             �	                                                                                          �          
  X
  �      �	  
    
                  �	  �
             D
                                                                                          �          
    �      �
                        l
  4             �
                                                                                          �            �  �      ,                          �             �                                                                                          �            \  �      �                        �  �             H                                                                                          �                �      �                        p               �                                                                                          �            p         �       }  X  �     �  }  �W               }             �          �      �              �       �  X  $     @  �  �o      �  	       �         �    $          �      �                 T�                                               X�          `  �  L l@                �         
             
             
                                         
                                                                                                               
             
                                          L   \   l   |   �   �   �   �   �   �   �   �       ,  <  L  \      L   \   l   |   �   �   �   �   �   �   �   �      ,  <  L  \                                                                                                                                     	                                 8  @  D  L  H          P             d  l  t  �  |          �             �  �  �  �  �          �             �  �  �                                      $                              (  0  8  @                             D  P  X  d                             h  t  |  �                                                                          CodCia  999 Cia Cia 0   C�digo de Compa�ia  CodAlm  x(3)    Almac�n Almac�n     C�digo de almac�n   Descripcion X(40)   Descripci�n Descripci�n     Descripci�n de almac�n  RowNum  ->,>>>,>>9  RowNum  0   RowIdent    x(8)    RowIdent        RowMod  x(8)    RowMod      RowIdentIdx x(8)    RowIdentIdx     RowUserProp x(8)    RowUserProp     �  ���	������                       %        ,                �     i     i     i     	 	 	    �    	      %  ,  8                                                                                                                                     	                  
                                 �  �  �  �  �          �             �  �  �  �  �          �                   4  (          8             P  X  d  l                             p  |  �  �                              �  �  �  �                             �  �  �  �                             �  �  �  �                              �                                                                                 CodCia  999 Cia Cia 0   C�digo de Compa�ia  CodAlm  x(3)    Almac�n Almac�n     C�digo de almac�n   Descripcion X(40)   Descripci�n Descripci�n     Descripci�n de almac�n  RowNum  ->,>>>,>>9  RowNum  0   RowIdent    x(8)    RowIdent        RowMod  x(8)    RowMod      RowIdentIdx x(8)    RowIdentIdx     RowUserProp x(8)    RowUserProp     ChangedFields   x(8)    ChangedFields       �  ���
������                       %        ,                �     i     i     i     	 	 	    �    	      %  ,  8  D    ��                            ����                            }    t�                    o�    undefined                                                               �       x�  x   `   ��  ��                    �����               �`e        O   ����    e�          O   ����    R�          O   ����    ��      d        �   �           4   ����     /      �                                3   ����       $       8  ���                       8      
                       � ߱        x  �   "   D       �     >          |�     �   �            4   ����d                 $                      ��                  �   �                   l�f           �   �  h  	  �   X                                        3   ����|       O   �   ��  ��  �   batchServices                                 �      ��                  S  V                 $��        O   ����    e�          O   ����    R�          O   ����    ��            ��   l             8               ��                  `           ��                            ����                            clientSendRows                              P  8      ��                  X  ^  h              ���        O   ����    e�          O   ����    R�          O   ����    ��            ��   �             �               ��   �             �               ��                �               ��   ,             �               ��                              ��                            ����                            commitTransaction                                 �      ��                  `  a  ,              �V�        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            copyColumns                               �      ��                  c  f                 lW�        O   ����    e�          O   ����    R�          O   ����    ��            ��   l             8               �� 
                 `  
         ��                            ����                            dataAvailable                               P  8      ��                  h  j  h              Џ        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            describeSchema                              p	  X	      ��                  l  o  �	              ��        O   ����    e�          O   ����    R�          O   ����    ��            ��   �	             �	               �� 
          �       �	  
         ��                            ����                            destroyServerObject                             �
  �
      ��                  q  r  �
              �ak        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            endClientDataRequest                                �  �      ��                  t  u  �              �bk        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchBatch                              �  �      ��                  w  y  �              Lck        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            fetchFirst                              �  �      ��                  {  |  �              (jk        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchLast                               �  �      ��                  ~    �              �jk        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchNext                               �  �      ��                  �  �  �              ,kk        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchPrev                               �  �      ��                  �  �  �              (��        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            genContextList                              �  �      ��                  �  �  �              ���        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            home                                �  �      ��                  �  �  �              0��        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �  �      ��                  �  �  �              (��        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              �  �      ��                  �  �  �              ���        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            isUpdatePending                             �  �      ��                  �  �  �              T��        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            printToCrystal                              �  �      ��                  �  �  �              (�        O   ����    e�          O   ����    R�          O   ����    ��            ��   0             �               ��   X             $               ��                  L           ��                            ����                            refreshRow                              8         ��                  �  �  P              (6f        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            remoteSendRows                              0        ��                  �  �  H              �6f        O   ����    e�          O   ����    R�          O   ����    ��            ��   �             `               ��   �             �               ��   �             �               ��                �               ��   4                             ��   \             (               �� 
  �      �       P  
             ��                  x           ��                            ����                            restartServerObject                             l  T      ��                  �  �  �              X�        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            retrieveFilter                              d  L      ��                  �  �  |              D+f        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            rowObjectState                              \  D      ��                  �  �  t              0�        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            saveContextAndDestroy                               �  l      ��                  �  �  �              xCf        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            serverSendRows                              �  �      ��                  �  �  �              (�        O   ����    e�          O   ����    R�          O   ����    ��            ��                 �               ��   0              �               ��   X              $                ��   �              L                ��   �              t                �� 
          �       �   
         ��                            ����                            serverFetchRowObjUpdTable                               �!  �!      ��                  �  �  �!              D�        O   ����    e�          O   ����    R�          O   ����    ��            �� 
          �       �!  
         ��                            ����                            setPropertyList                             �"  �"      ��                  �  �  �"              ��        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �"           ��                            ����                            serverSendRows                              �#  �#      ��                  �  �  �#              �bm        O   ����    e�          O   ����    R�          O   ����    ��            ��   <$             $               ��   d$             0$               ��   �$             X$               ��   �$             �$               ��   �$             �$               �� 
          �       �$  
         ��                            ����                            startServerObject                               �%  �%      ��                  �  �  �%              h�        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            submitCommit                                �&  �&      ��                  �  �  �&              �        O   ����    e�          O   ����    R�          O   ����    ��            ��    '             �&               ��                  '           ��                            ����                            submitForeignKey                                (  �'      ��                  �  �   (              �        O   ����    e�          O   ����    R�          O   ����    ��            ��   l(             8(               ��   �(             `(               ��                  �(           ��                            ����                            submitValidation                                |)  d)      ��                  �  �  �)              ��o        O   ����    e�          O   ����    R�          O   ����    ��            ��   �)             �)               ��                  �)           ��                            ����                            synchronizeProperties                               �*  �*      ��                  �  �  �*              ��o        O   ����    e�          O   ����    R�          O   ����    ��            ��   0+             �*               ��                  $+           ��                            ����                            transferToExcel                             ,  �+      ��                    
  ,,              �o        O   ����    e�          O   ����    R�          O   ����    ��            ��   x,             D,               ��   �,             l,               ��   �,             �,               ��                  �,           ��                            ����                            undoTransaction                             �-  �-      ��                      �-              $�f        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            updateAddQueryWhere                             �.  �.      ��                      �.              ��f        O   ����    e�          O   ����    R�          O   ����    ��            ��   /             �.               ��                   /           ��                            ����                            updateQueryPosition                             �/  �/      ��                      0              ��f        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            updateState                             �0  �0      ��                       1              ��f        O   ����    e�          O   ����    R�          O   ����    ��            ��                  1           ��                            ����                            addRow          �1      �1     U       CHARACTER,INPUT pcViewColList CHARACTER cancelRow   �1      �1      �1   	 \       CHARACTER,  canNavigate �1      2      42    f       LOGICAL,    closeQuery  2      @2      l2   
 r       LOGICAL,    columnProps L2      x2      �2    }       CHARACTER,INPUT pcColList CHARACTER,INPUT pcPropList CHARACTER  colValues   �2      �2      3   	 �       CHARACTER,INPUT pcViewColList CHARACTER copyRow �2      83      `3    �       CHARACTER,INPUT pcViewColList CHARACTER createRow   @3      �3      �3   	 �       LOGICAL,INPUT pcValueList CHARACTER deleteRow   �3      �3      4   	 �       LOGICAL,INPUT pcRowIdent CHARACTER  fetchRow    �3      (4      T4  	  �       CHARACTER,INPUT piRow INTEGER,INPUT pcViewColList CHARACTER fetchRowIdent   44      �4      �4  
  �       CHARACTER,INPUT pcRowIdent CHARACTER,INPUT pcViewColList CHARACTER  findRow �4      5      ,5    �       LOGICAL,INPUT pcKeyValues CHARACTER findRowWhere    5      P5      �5    �       LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER  firstRowIds `5      �5      6    �       CHARACTER,INPUT pcQueryString CHARACTER getLastCommitErrorType  �5      ,6      d6    �       CHARACTER,  hasForeignKeyChanged    D6      p6      �6    �       LOGICAL,    openDataQuery   �6      �6      �6          LOGICAL,INPUT pcPosition CHARACTER  openQuery   �6      7      47   	 !      LOGICAL,    prepareQuery    7      @7      p7    +      LOGICAL,INPUT pcQuery CHARACTER rowAvailable    P7      �7      �7    8      LOGICAL,INPUT pcDirection CHARACTER rowValues   �7      �7      8   	 E      CHARACTER,INPUT pcColumns CHARACTER,INPUT pcFormat CHARACTER,INPUT pcDelimiter CHARACTER    submitRow   �7      l8      �8   	 O      LOGICAL,INPUT pcRowIdent CHARACTER,INPUT pcValueList CHARACTER  updateRow   x8      �8      9   	 Y      LOGICAL,INPUT pcKeyValues CHARACTER,INPUT pcValueList CHARACTER getObjectType   �8      D9      t9    c      CHARACTER,  assignDBRow                             :  �9      ��                  �    :              �~        O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 4:  
         ��                            ����                            bufferCopyDBToRO                                (;  ;      ��                      @;              ��f        O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �;             X;  
             �� 
  �;             �;  
             ��   �;             �;               ��                  �;           ��                            ����                            compareDBRow                                �<  �<      ��                  
    �<               �f        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            confirmContinue                             �=  �=      ��                      �=              t�f        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �=           ��                            ����                            dataAvailable                               �>  �>      ��                      �>              �f        O   ����    e�          O   ����    R�          O   ����    ��            ��                  ?           ��                            ����                            fetchDBRowForUpdate                             �?  �?      ��                      @              ,�f        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchFirst                              �@  �@      ��                      A              ܺf        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchLast                               �A  �A      ��                      �A              �F�        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchNext                               �B  �B      ��                      �B              0J�        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchPrev                               �C  �C      ��                  !  "  �C              �J�        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            filterContainerHandler                              �D  �D      ��                  $  &  �D              0N�        O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �D  
         ��                            ����                            initializeObject                                �E  �E      ��                  (  )  F              �R�        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            refetchDBRow                                �F  �F      ��                  +  -   G              0V�        O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 G  
         ��                            ����                            releaseDBRow                                H  �G      ��                  /  0   H              LZ�        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            startFilter                             �H  �H      ��                  2  3  I              �Z�        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            transferDBRow                               �I  �I      ��                  5  8  J              0^�        O   ����    e�          O   ����    R�          O   ����    ��            ��   XJ             $J               ��                  LJ           ��                            ����                            addQueryWhere   T9      �J      �J    �      LOGICAL,INPUT pcWhere CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER    assignQuerySelection    �J      8K      pK    �      LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER  bufferCompareDBToRO PK      �K      �K    �      LOGICAL,INPUT phRowObjUpd HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER bufferWhereClause   �K      hL      �L    �      CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcWhere CHARACTER  columnDataType  |L      �L      M    �      CHARACTER,INPUT pcColumn CHARACTER  columnDbColumn  �L      ,M      \M    �      CHARACTER,INPUT pcColumn CHARACTER  columnQuerySelection    <M      �M      �M    �      CHARACTER,INPUT pcColumn CHARACTER  columnTable �M      �M      N           CHARACTER,INPUT pcColumn CHARACTER  columnValMsg    �M      ,N      \N           CHARACTER,INPUT pcColumn CHARACTER  dbColumnDataName    <N      �N      �N  !        CHARACTER,INPUT pcDbColumn CHARACTER    dbColumnHandle  �N      �N      O  "  *      HANDLE,INPUT pcColumn CHARACTER excludeColumns  �N      ,O      \O  #  9      CHARACTER,INPUT iTable INTEGER  getDataColumns  <O      |O      �O  $  H      CHARACTER,  getForeignValues    �O      �O      �O  %  W      CHARACTER,  getQueryPosition    �O      �O      ,P  &  h      CHARACTER,  getQuerySort    P      8P      hP  '  y      CHARACTER,  getQueryString  HP      tP      �P  (  �      CHARACTER,  getQueryWhere   �P      �P      �P  )  �      CHARACTER,  getTargetProcedure  �P      �P       Q  *  �      HANDLE, indexInformation     Q      (Q      \Q  +  �      CHARACTER,INPUT pcQuery CHARACTER,INPUT plUseTableSep LOGICAL,INPUT pcIndexInfo CHARACTER   insertExpression    <Q      �Q      �Q  ,  �      CHARACTER,INPUT pcWhere CHARACTER,INPUT pcExpression CHARACTER,INPUT pcAndOr CHARACTER  newQueryString  �Q      DR      tR  -  �      CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcAndOr CHARACTER  newQueryValidate    TR      S      8S  .  �      CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcExpression CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER   newQueryWhere   S      �S      �S  /  �      CHARACTER,INPUT pcWhere CHARACTER   newWhereClause  �S      T      4T  0        CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcExpression CHARACTER,INPUT pcWhere CHARACTER,INPUT pcAndOr CHARACTER refreshRowident T      �T      �T  1        CHARACTER,INPUT pcRowident CHARACTER    removeForeignKey    �T      �T      0U  2  %      LOGICAL,    removeQuerySelection    U      <U      tU  3  6      LOGICAL,INPUT pcColumns CHARACTER,INPUT pcOperators CHARACTER   resolveBuffer   TU      �U      �U  4  K      CHARACTER,INPUT pcBuffer CHARACTER  rowidWhere  �U      V      4V  5 
 Y      CHARACTER,INPUT pcWhere CHARACTER   rowidWhereCols  V      XV      �V  6  d      CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER    setQueryPosition    hV      �V      W  7  s      LOGICAL,INPUT pcPosition CHARACTER  setQuerySort    �V      <W      lW  8  �      LOGICAL,INPUT pcSort CHARACTER  setQueryString  LW      �W      �W  9  �      LOGICAL,INPUT pcQueryString CHARACTER   setQueryWhere   �W      �W      X  :  �      LOGICAL,INPUT pcWhere CHARACTER whereClauseBuffer   �W      4X      hX  ;  �      CHARACTER,INPUT pcWhere CHARACTER   bindServer                              Y  �X      ��                  �  �  (Y              Py�        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               Z  �Y      ��                  �  �   Z              �{�        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyServerObject                             [  �Z      ��                  �  �  [              �|�        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disconnectObject                                 \  �[      ��                  �  �  \              ��        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                               ]  �\      ��                  �  �  ]              ���        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            restartServerObject                             �]  �]      ��                  �  �  ^              ���        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            runServerObject                             �^  �^      ��                  �  �  _              D��        O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 $_  
         ��                            ����                            startServerObject                               `   `      ��                  �  �  0`              ���        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            unbindServer                                a  �`      ��                  �  �  (a              0��        O   ����    e�          O   ����    R�          O   ����    ��            ��                  @a           ��                            ����                            getAppService   HX      �a      �a  <  �      CHARACTER,  getASBound  �a      �a      b  = 
 �      LOGICAL,    getAsDivision   �a      b      Lb  >  �      CHARACTER,  getASHandle ,b      Xb      �b  ?  �      HANDLE, getASHasStarted db      �b      �b  @  �      LOGICAL,    getASInfo   �b      �b      �b  A 	       CHARACTER,  getASInitializeOnRun    �b       c      8c  B        LOGICAL,    getASUsePrompt  c      Dc      tc  C  "      LOGICAL,    getServerFileName   Tc      �c      �c  D  1      CHARACTER,  getServerOperatingMode  �c      �c      �c  E  C      CHARACTER,  runServerProcedure  �c      d      8d  F  Z      HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE   setAppService   d      |d      �d  G  m      LOGICAL,INPUT pcAppService CHARACTER    setASDivision   �d      �d      e  H  {      LOGICAL,INPUT pcDivision CHARACTER  setASHandle �d      (e      Te  I  �      LOGICAL,INPUT phASHandle HANDLE setASInfo   4e      te      �e  J 	 �      LOGICAL,INPUT pcInfo CHARACTER  setASInitializeOnRun    �e      �e      �e  K  �      LOGICAL,INPUT plInitialize LOGICAL  setASUsePrompt  �e      f      Lf  L  �      LOGICAL,INPUT plFlag LOGICAL    setServerFileName   ,f      lf      �f  M  �      LOGICAL,INPUT pcFileName CHARACTER  setServerOperatingMode  �f      �f      �f  N  �      LOGICAL,INPUT pcServerOperatingMode CHARACTER   addLink                             �g  �g      ��                  �  �  �g              �n        O   ����    e�          O   ����    R�          O   ����    ��            �� 
  h             �g  
             ��   8h             h               �� 
                 ,h  
         ��                            ����                            addMessage                              i   i      ��                  �  �  0i              p�        O   ����    e�          O   ����    R�          O   ����    ��            ��   |i             Hi               ��   �i             pi               ��                  �i           ��                            ����                            adjustTabOrder                              �j  pj      ��                  �  �  �j              �        O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �j             �j  
             �� 
  k             �j  
             ��                  k           ��                            ����                            applyEntry                              �k  �k      ��                  �  �  l              ��        O   ����    e�          O   ����    R�          O   ����    ��            ��                  $l           ��                            ����                            changeCursor                                m  �l      ��                  �  �  ,m              ,�        O   ����    e�          O   ����    R�          O   ����    ��            ��                  Dm           ��                            ����                            createControls                              4n  n      ��                  �  �  Ln              @�        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               ,o  o      ��                  �  �  Do              $�        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            displayLinks                                $p  p      ��                  �  �  <p              �        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            editInstanceProperties                              $q  q      ��                  �  �  <q              p�        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            exitObject                              r   r      ��                  �  �  0r              �        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hideObject                              s  �r      ��                  �  �  $s              ��        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                t  �s      ��                  �  �   t              ��        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            modifyListProperty                              u  �t      ��                  �  �  u              ��        O   ����    e�          O   ����    R�          O   ����    ��            �� 
  hu             4u  
             ��   �u             \u               ��   �u             �u               ��                  �u           ��                            ����                            modifyUserLinks                             �v  �v      ��                  �  �  �v              0_f        O   ����    e�          O   ����    R�          O   ����    ��            ��    w             �v               ��   (w             �v               �� 
                 w  
         ��                            ����                            removeAllLinks                              x  �w      ��                  �  �  $x              �ff        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            removeLink                               y  �x      ��                  �  �  y              gf        O   ����    e�          O   ����    R�          O   ����    ��            �� 
  dy             0y  
             ��   �y             Xy               �� 
                 �y  
         ��                            ����                            repositionObject                                tz  \z      ��                  �  �  �z              �of        O   ����    e�          O   ����    R�          O   ����    ��            ��   �z             �z               ��                  �z           ��                            ����                            returnFocus                             �{  �{      ��                  �  �  �{              |vf        O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �{  
         ��                            ����                            showMessageProcedure                                �|  �|      ��                  �    �|              �vf        O   ����    e�          O   ����    R�          O   ����    ��            ��   D}             }               ��                  8}           ��                            ����                            toggleData                              $~  ~      ��                      <~              4�f        O   ����    e�          O   ����    R�          O   ����    ��            ��                  T~           ��                            ����                            viewObject                              @  (      ��                      X              H�f        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            anyMessage  �f      �      �  O 
 :      LOGICAL,    assignLinkProperty  �      �      �  P  E      LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER   fetchMessages   �      t�      ��  Q  X      CHARACTER,  getChildDataKey ��      ��      ��  R  f      CHARACTER,  getContainerHandle  ��      �       �  S  v      HANDLE, getContainerHidden   �      (�      \�  T  �      LOGICAL,    getContainerSource  <�      h�      ��  U  �      HANDLE, getContainerSourceEvents    |�      ��      ��  V  �      CHARACTER,  getContainerType    ��      �       �  W  �      CHARACTER,  getDataLinksEnabled  �      ,�      `�  X  �      LOGICAL,    getDataSource   @�      l�      ��  Y  �      HANDLE, getDataSourceEvents |�      ��      ؂  Z  �      CHARACTER,  getDataSourceNames  ��      �      �  [        CHARACTER,  getDataTarget   ��      $�      T�  \  "      CHARACTER,  getDataTargetEvents 4�      `�      ��  ]  0      CHARACTER,  getDBAware  t�      ��      ̃  ^ 
 D      LOGICAL,    getDesignDataObject ��      ؃      �  _  O      CHARACTER,  getDynamicObject    �      �      L�  `  c      LOGICAL,    getInstanceProperties   ,�      X�      ��  a  t      CHARACTER,  getLogicalObjectName    p�      ��      Ԅ  b  �      CHARACTER,  getLogicalVersion   ��      ��      �  c  �      CHARACTER,  getObjectHidden �       �      P�  d  �      LOGICAL,    getObjectInitialized    0�      \�      ��  e  �      LOGICAL,    getObjectName   t�      ��      Ѕ  f  �      CHARACTER,  getObjectPage   ��      ܅      �  g  �      INTEGER,    getObjectParent �      �      H�  h  �      HANDLE, getObjectVersion    (�      P�      ��  i        CHARACTER,  getObjectVersionNumber  d�      ��      Ȇ  j        CHARACTER,  getParentDataKey    ��      Ԇ      �  k  *      CHARACTER,  getPassThroughLinks �      �      H�  l  ;      CHARACTER,  getPhysicalObjectName   (�      T�      ��  m  O      CHARACTER,  getPhysicalVersion  l�      ��      ̇  n  e      CHARACTER,  getPropertyDialog   ��      ؇      �  o  x      CHARACTER,  getQueryObject  �      �      H�  p  �      LOGICAL,    getRunAttribute (�      T�      ��  q  �      CHARACTER,  getSupportedLinks   d�      ��      Ĉ  r  �      CHARACTER,  getTranslatableProperties   ��      Ј      �  s  �      CHARACTER,  getUIBMode  �      �      D�  t 
 �      CHARACTER,  getUserProperty $�      P�      ��  u  �      CHARACTER,INPUT pcPropName CHARACTER    instancePropertyList    `�      ��      ��  v  �      CHARACTER,INPUT pcPropList CHARACTER    linkHandles ��      �      4�  w  	      CHARACTER,INPUT pcLink CHARACTER    linkProperty    �      X�      ��  x  	      CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER mappedEntry h�      Ċ      ��  y  	      CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER  messageNumber   Њ      \�      ��  z  *	      CHARACTER,INPUT piMessage INTEGER   propertyType    l�      ��      ��  {  8	      CHARACTER,INPUT pcPropName CHARACTER    reviewMessages  ��      �      8�  |  E	      CHARACTER,  setChildDataKey �      D�      t�  }  T	      LOGICAL,INPUT cChildDataKey CHARACTER   setContainerHidden  T�      ��      Ќ  ~  d	      LOGICAL,INPUT plHidden LOGICAL  setContainerSource  ��      ��      $�    w	      LOGICAL,INPUT phObject HANDLE   setContainerSourceEvents    �      D�      ��  �  �	      LOGICAL,INPUT pcEvents CHARACTER    setDataLinksEnabled `�      ��      ؍  �  �	      LOGICAL,INPUT lDataLinksEnabled LOGICAL setDataSource   ��       �      0�  �  �	      LOGICAL,INPUT phObject HANDLE   setDataSourceEvents �      P�      ��  �  �	      LOGICAL,INPUT pcEventsList CHARACTER    setDataSourceNames  d�      ��      ��  �  �	      LOGICAL,INPUT pcSourceNames CHARACTER   setDataTarget   ��      �      8�  �  �	      LOGICAL,INPUT pcTarget CHARACTER    setDataTargetEvents �      \�      ��  �  �	      LOGICAL,INPUT pcEvents CHARACTER    setDBAware  p�      ��      ��  � 
 
      LOGICAL,INPUT lAware LOGICAL    setDesignDataObject ��       �      4�  �  
      LOGICAL,INPUT pcDataObject CHARACTER    setDynamicObject    �      \�      ��  �  -
      LOGICAL,INPUT lTemp LOGICAL setInstanceProperties   p�      ��      �  �  >
      LOGICAL,INPUT pcPropList CHARACTER  setLogicalObjectName    Đ      �      @�  �  T
      LOGICAL,INPUT c CHARACTER   setLogicalVersion    �      \�      ��  �  i
      LOGICAL,INPUT cVersion CHARACTER    setObjectName   p�      ��      �  �  {
      LOGICAL,INPUT pcName CHARACTER  setObjectParent đ      �      4�  �  �
      LOGICAL,INPUT phParent HANDLE   setObjectVersion    �      T�      ��  �  �
      LOGICAL,INPUT cObjectVersion CHARACTER  setParentDataKey    h�      ��      �  �  �
      LOGICAL,INPUT cParentDataKey CHARACTER  setPassThroughLinks Ē      �      @�  �  �
      LOGICAL,INPUT pcLinks CHARACTER setPhysicalObjectName    �      `�      ��  �  �
      LOGICAL,INPUT cTemp CHARACTER   setPhysicalVersion  x�      ��      �  �  �
      LOGICAL,INPUT cVersion CHARACTER    setRunAttribute ̓      �      @�  �  �
      LOGICAL,INPUT cRunAttribute CHARACTER   setSupportedLinks    �      h�      ��  �        LOGICAL,INPUT pcLinkList CHARACTER  setTranslatableProperties   |�      ��      ��  �        LOGICAL,INPUT pcPropList CHARACTER  setUIBMode  ܔ       �      L�  � 
 4      LOGICAL,INPUT pcMode CHARACTER  setUserProperty ,�      l�      ��  �  ?      LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER  showmessage |�      ܕ      �  �  O      LOGICAL,INPUT pcMessage CHARACTER   Signature   �      ,�      X�  � 	 [      CHARACTER,INPUT pcName CHARACTER     �       ��  �          4   �����                 �                      ��                    L                  �z�             ��            ,�  ��          4   �����                 ��                      ��                  !  K                  {�           !  <�  ��     8  ė  4�          4   �����                 D�                      ��                  D  F                  ��           D  ԗ         E                                  ,     
                    � ߱        Ș  $   H  p�  ���                           $   J  ��  ���                       x                         � ߱        �     P  8�  ��          4   �����                ��                      ��                  Q  	                  ��           Q  H�  �  o   T      ,                                 D�  $   U  �  ���                       �  @         �              � ߱        X�  �   V        l�  �   W  �      ��  �   Y        ��  �   [  x      ��  �   ]  �      ��  �   _  `      К  �   `  �      �  �   a        ��  �   d  �      �  �   f          �  �   g  |      4�  �   i  �      H�  �   j  t      \�  �   k  �      p�  �   l  ,      ��  �   m  �      ��  �   s  �      ��  �   u  P	      ��  �   {  �	      ԛ  �   }   
      �  �     t
      ��  �   �  �
      �  �   �  l      $�  �   �  �      8�  �   �  \      L�  �   �  �      `�  �   �  D      t�  �   �  �      ��  �   �  �      ��  �   �  0      ��  �   �  �      Ĝ  �   �  �      ؜  �   �        �  �   �  X       �  �   �  �      �  �   �        (�  �   �  L      <�  �   �  �      P�  �   �  �      d�  �   �         x�  �   �  <      ��  �   �  x      ��  �   �  �      ��  �   �  �          �   �  ,                      Ԟ          @�  (�      ��                  <	  j	  X�              |)        O   ����    e�          O   ����    R�          O   ����    ��      �     
                                     (                         � ߱         �  $  P	  p�  ���                           O   h	  ��  ��  h               l�          \�  d�    L�                                             ��                            ����                                D9      ȝ      �     V     t�                       p�  c                     ��     �	  (�  ��          4   ����t                ��                      ��                  �	  
                  l&           �	  8�  ��  �   �	  �      Р  �   �	  H      �  �   �	  �      ��  �   �	  @      �  �   �	  �       �  �   �	  8      4�  �   �	  �      H�  �   �	  (      \�  �   �	  �      p�  �   �	         ��  �   �	  �      ��  �   �	        ��  �   �	  �          �   �	        �     A
  ء  H�          4   ����x                X�                      ��                  B
  �
                  ��g           B
  �  l�  �   D
  �      ��  �   E
  T      ��  �   F
  �      ��  �   G
  D      ��  �   H
  �      Т  �   I
  �      �  �   K
  p      ��  �   L
  �      �  �   M
  X       �  �   N
  �      4�  �   O
  �      H�  �   P
  D       \�  �   Q
  �       p�  �   R
  �       ��  �   S
  x!      ��  �   T
  �!      ��  �   U
  h"      ��  �   V
  �"      ԣ  �   W
  `#      �  �   X
  �#      ��  �   Y
  X$      �  �   Z
  �$      $�  �   [
  �$      8�  �   \
  L%      L�  �   ]
  �%      `�  �   ^
  <&      t�  �   _
  �&      ��  �   `
  4'      ��  �   a
  �'      ��  �   b
  ,(      Ĥ  �   c
  h(      ؤ  �   e
  �(      �  �   f
  X)       �  �   g
  �)      �  �   h
  *      (�  �   i
  �*      <�  �   j
  �*      P�  �   k
  l+      d�  �   l
  �+      x�  �   m
  \,      ��  �   n
  �,      ��  �   o
  L-      ��  �   p
  �-      ȥ  �   q
  <.      ܥ  �   r
  �.      �  �   s
  4/      �  �   t
  �/          �   u
  $0      ̨     �
  0�  ��          4   ����T0                ��                      ��                  �
  ~                  d�k           �
  @�  Ħ  �   �
  �0      ئ  �   �
  (1      �  �   �
  �1       �  �   �
  2      �  �   �
  �2      (�  �   �
  3      <�  �   �
  |3      P�  �   �
  �3      d�  �   �
  t4      x�  �   �
  �4      ��  �   �
  l5      ��  �   �
  �5      ��  �   �
  d6      ȧ  �      �6      ܧ  �     L7      �  �     �7      �  �     <8      �  �     �8      ,�  �     ,9      @�  �     �9      T�  �     :      h�  �     X:      |�  �   	  �:      ��  �   
  H;      ��  �     �;      ��  �     8<          �     �<      Ы     �  �  T�          4   ����=  	              d�                      ��             	     �  -                  T�           �  ��  x�  �   �  |=      ��  �   �  �=      ��  �   �  t>      ��  �   �  �>      ȩ  �   �  l?      ܩ  �   �  �?      �  �   �  \@      �  �   �  �@      �  �   �  TA      ,�  �   �  �A      @�  �   �  DB      T�  �   �  �B      h�  �   �  <C      |�  �   �  �C      ��  �   �  ,D      ��  �   �  �D      ��  �   �  $E      ̪  �   �  �E      �  �   �  F      ��  �   �  �F      �  �   �  G      �  �   �  �G      0�  �   �  �G      D�  �   �  8H      X�  �   �  �H      l�  �   �  0I      ��  �   �  �I      ��  �   �   J          �   �  �J      getRowObjUpdStatic  deleteRecordStatic  d�     �  �  ��          4   ����K      /   �  $�     4�                          3   ����K            T�                      3   ����<K  �     �  |�  �  <�      4   ����XK  
              ��                      ��             
     �  O                  4��           �  ��  �  �   �  �K      h�  $   �  <�  ���                       �K     
                    � ߱        |�  �   �  L      ԭ  $   �  ��  ���                       ,L  @         L              � ߱        ��  $   �   �  ���                       �L       	       	           � ߱        �M     
                N                     \O  @        
 O              � ߱         �  V     ,�  ���                        hO       	       	       �O       
       
       �O       	       	           � ߱        ��  $     ��  ���                       �P     
                Q                     dR  @        
 $R              � ߱            V   0  L�  ���                                      �                      ��                  Q  �                  ���           Q  ܯ  pR     
                �R                     <T  @        
 �S          �T  @        
 dT          U  @        
 �T          dU  @        
 $U              � ߱            V   f  L�  ���                        adm-clone-props ��  0�              �     W     4                          0  W                     start-super-proc    @�  ��  �           �     X                                  x                     ��       $�  4�          4   �����X      /     `�     p�                          3   ���� Y            ��                      3   ���� Y  ��  $   !  ̲  ���                       @Y                         � ߱        ̴     1  �  ��   �      4   ����\Y                ��                      ��                  2  6                  ���           2   �  pY                     �Y                     �Y                         � ߱            $   3  ��  ���                              7  8�  t�          4   �����Y  �Y                         � ߱            $   8  H�  ���                       �Y                         � ߱        ��  $   <  ��  ���                       �     ?  �   �  x�      4   �����Y      $   @  L�  ���                       Z                         � ߱            �   ]  ,Z      lZ     
                �Z                     8\  @        
 �[              � ߱        �  V   q  ��  ���                        0�  �   �  D\      (�     #  H�  X�          4   �����\      /   $  ��     ��                          3   �����\            ��                      3   �����\  �\     
                P]                     �^  @        
 `^              � ߱        ��  V   0  Ķ  ���                        �^     
                h_                     �`  @        
 x`              � ߱        �  V   T  T�  ���                        T�     �  ��  l�          4   �����`                |�                      ��                  �  �                  |��           �  �  �  /   �  ��     ��                          3   �����`            ظ                      3   �����`      /   �  �     $�                          3   ����a            D�                      3   ����8a  �  /  A  ��         la                      3   ����Ta  initProps   ��  ��              D     Y     <                          8  �  	                                   Ⱥ          p�  X�      ��                 �    ��              ��k        O   ����    e�          O   ����    R�          O   ����    ��      �                      ��          ��  p     �r  �        d�  T�     �r                                        ��                                      T��             ��  �  Ի     �r                                        ��                    ;                  ���             t�  d�  T�     �r                                        ��                  <  X                  ���           <  ��  �  Լ     �r                                        ��                  Y  u                  `��           Y  t�  d�  T�     s                                        ��                  v  �                  d��           v  ��  �  Խ     $s                                        ��                  �  �                  4��           �  t�  d�  T�     8s                                        ��                  �  �                  ��           �  ��  �  Ծ     Ls                                        ��                  �  �                  ԗ�           �  t�  d�  T�     `s  	                                      ��             	     �                    ���           �  ��  �  Կ     ts  
                                      ��             
       #                  ���             t�  d�  T�     �s                                        ��                  $  @                  Ȫ�           $  ��  ��  ��     �s                                        ��                  A  ]                  ���           A  t�  d�  T�     �s                                        ��                  ^  z                  h��           ^  ��  ��  ��     �s                                        ��                  {  �                  ���           {  t�  d�  T�     �s                                        ��                  �  �                  <��           �  ��  ��  ��     �s                                        ��                  �  �                  ��           �  t�  d�  T�      t                                        ��                  �  �                  ܯ�           �  ��      ��     t                                        ��                  �                    ���           �  t�      O     ��  ��  (t               h�          P�  \�   , 0�                                                       �     ��                            ����                            ��  ��  ��  ��      ��     Z     p�                      � l�  �                     ��     $  $�  ��          4   ����4t                ��                      ��                  %  9                  ,��           %  4�  �  /   &  ��     ��                          3   ����Dt             �                      3   ����dt  |�  /   '  <�     L�                          3   ����|t            l�                      3   �����t  ��  /   ,  ��     ��                          3   �����t            ��                      3   �����t      /   2  �     $�                          3   �����t            D�                      3   ����u  8u     
                �u                     w  @        
 �v              � ߱        ��  V   �  T�  ���                        ��  $   �  �  ���                       w                         � ߱        4w     
                �w                      y  @        
 �x              � ߱        ��  V   �  <�  ���                        ��  $   �  ��  ���                       y     
                    � ߱         y     
                �y                     �z  @        
 �z              � ߱        ��  V   �  $�  ���                        p�  $   �  ��  ���                       �z     
                    � ߱        {     
                �{                     �|  @        
 �|              � ߱        ��  V   �  �  ���                        X�  $     ��  ���                       �|                         � ߱        }     
                �}                     �~  @        
 �~              � ߱        ��  V     ��  ���                        ��  �   1  �~      T�  $   2  ��  ���                            
                    � ߱        0     
                �                     ��  @        
 ��              � ߱        ��  V   <  ��  ���                        ��  $   V  ��  ���                       �     
                    � ߱        ��  �   p  �      D�  $   z  �  ���                       \�     
                    � ߱        X�  �   �  p�      ��  $   �  ��  ���                       ��                         � ߱               �  ��  ��          4   ����́      /   �  �     �                          3   �����  D�     
   4�                      3   �����  t�        d�                      3   �����  ��        ��                      3   ����(�            ��                      3   ����D�  pushRowObjUpdTable  ��  ��  �                   [      �                               �                     pushTableAndValidate    ��  D�  �           p     \     �                          �  �                     remoteCommit    \�  ��  �           d     ]     �                          �  E                     serverCommit    ��  $�  �           `     ^     �                          �  R                                     8�          �  ��      ��                  �  �   �              l-�        O   ����    e�          O   ����    R�          O   ����    ��          O   �  ��  ��  t�    ��                            ����                            4�  ��      ��              _      P�                      
�     _                     disable_UI  ��  ��                      `      �                               r  
                    �  �    ����  �       ��       ��  8   ����   ��  8   ����       8   ����       8   ����       ��  ��      viewObject  ,   ��  ��  ��      toggleData  ,INPUT plEnabled LOGICAL    ��   �  8�      showMessageProcedure    ,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL  �  |�  ��      returnFocus ,INPUT hTarget HANDLE   l�  ��  ��      repositionObject    ,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL    ��   �  �      removeLink  ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE ��  `�  p�      removeAllLinks  ,   P�  ��  ��      modifyUserLinks ,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE t�  ��   �      modifyListProperty  ,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER    ��  x�  ��      hideObject  ,   h�  ��  ��      exitObject  ,   ��  ��  ��      editInstanceProperties  ,   ��  ��  ��      displayLinks    ,   ��  �  �      createControls  ,   ��  ,�  <�      changeCursor    ,INPUT pcCursor CHARACTER   �  h�  t�      applyEntry  ,INPUT pcField CHARACTER    X�  ��  ��      adjustTabOrder  ,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER ��  �  �      addMessage  ,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER ��  l�  t�      addLink ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE \�  ��  ��      unbindServer    ,INPUT pcMode CHARACTER ��   �  �      runServerObject ,INPUT phAppService HANDLE  ��  <�  P�      disconnectObject    ,   ,�  d�  t�      destroyObject   ,   T�  ��  ��      bindServer  ,   x�  ��  ��      transferDBRow   ,INPUT pcRowIdent CHARACTER,INPUT piRowNum INTEGER  ��  ��  �      startFilter ,   ��  �  ,�      releaseDBRow    ,   �  @�  P�      refetchDBRow    ,INPUT phRowObjUpd HANDLE   0�  |�  ��      filterContainerHandler  ,INPUT phFilterContainer HANDLE l�  ��  ��      fetchDBRowForUpdate ,   ��  ��  ��      confirmContinue ,INPUT-OUTPUT pioCancel LOGICAL ��  ,�  <�      compareDBRow    ,   �  P�  d�      bufferCopyDBToRO    ,INPUT phRowObj HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER   @�  ��  ��      assignDBRow ,INPUT phRowObjUpd HANDLE   ��  �  �      updateState ,INPUT pcState CHARACTER     �  H�  \�      updateQueryPosition ,   8�  p�  ��      updateAddQueryWhere ,INPUT pcWhere CHARACTER,INPUT pcField CHARACTER    `�  ��  ��      undoTransaction ,   ��  ��  ��      transferToExcel ,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT plUseExisting LOGICAL,INPUT piMaxRecords INTEGER  ��  |�  ��      synchronizeProperties   ,INPUT pcPropertiesForServer CHARACTER,OUTPUT pcPropertiesForClient CHARACTER   l�  ��  �      submitValidation    ,INPUT pcValueList CHARACTER,INPUT pcUpdColumns CHARACTER   ��  T�  h�      submitForeignKey    ,INPUT pcRowIdent CHARACTER,INPUT-OUTPUT pcValueList CHARACTER,INPUT-OUTPUT pcUpdColumns CHARACTER  D�  ��  ��      submitCommit    ,INPUT pcRowIdent CHARACTER,INPUT plReopen LOGICAL  ��  0�  D�      startServerObject   ,    �  X�  h�      setPropertyList ,INPUT pcProperties CHARACTER   H�  ��  ��      serverFetchRowObjUpdTable   ,OUTPUT TABLE-HANDLE phRowObjUpd    ��  ��  ��      serverSendRows  ,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER,OUTPUT TABLE-HANDLE phRowObject    ��  ��  ��      saveContextAndDestroy   ,OUTPUT pcContext CHARACTER ��  ��  �      rowObjectState  ,INPUT pcState CHARACTER    ��  0�  @�      retrieveFilter  ,    �  T�  h�      restartServerObject ,   D�  |�  ��      remoteSendRows  ,INPUT-OUTPUT piocContext CHARACTER,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT pioRowsReturned INTEGER,OUTPUT TABLE-HANDLE phRowObject,OUTPUT pocMessages CHARACTER   l�  ��  ��      refreshRow  ,   t�  ��  ��      printToCrystal  ,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT piMaxRecords INTEGER  ��  �  (�      isUpdatePending ,INPUT-OUTPUT plUpdate LOGICAL  �  X�  p�      initializeServerObject  ,   H�  ��  ��      initializeObject    ,   t�  ��  ��      home    ,   ��  ��  ��      genContextList  ,OUTPUT pcContext CHARACTER ��  �  �      fetchPrev   ,   ��  $�  0�      fetchNext   ,   �  D�  P�      fetchLast   ,   4�  d�  p�      fetchFirst  ,   T�  ��  ��      fetchBatch  ,INPUT plForwards LOGICAL   t�  ��  ��      endClientDataRequest    ,   ��  ��  ��      destroyServerObject ,   ��  �   �      describeSchema  ,INPUT pcSdoName CHARACTER,OUTPUT TABLE-HANDLE hTtSchema     �  l�  |�      dataAvailable   ,INPUT pcRelative CHARACTER \�  ��  ��      copyColumns ,INPUT pcViewColList CHARACTER,INPUT phDataQuery HANDLE ��  ��  �      commitTransaction   ,   ��  $�  4�      clientSendRows  ,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER    �  ��  ��      batchServices   ,INPUT pcServices CHARACTER,OUTPUT pcValues CHARACTER        � 
"     
 n%     adecomm/as-utils.w  
"   
   �    }        �
"     
    �     }        ��    D   %               � 
"    
 � %              h �P  \         (          
�                          
�            � e   �
"    
 
�H T   %              �     }        �GG %              � 
"   
   P �L 
�H T   %              �     }        �GG %              
"   
   �        �    7%               
"   
 �               1� u  
 � �   � %               o%   o           � �    
"   
 �           �    1� �   � �   � %               o%   o           � �   
"   
 �           �    1� �  
 � �   � %               o%   o           � �   
"   
 �           l    1� �   � �   � %               o%   o           � �    
"   
 �           �    1� �   � �   � %               o%   o           � �   
"   
 �           T    1� �   � �   � %               o%   o           %               
"   
 � �          �    1� �   � �      
"   
 �               1�    � �   � %               o%   o           � &  
"   
 �           �    1� (   � �   � %               o%   o           � 7  S 
"   
 �           �    1� �   � �   � %               o%   o           %               
"   
 �           p    1� �   � �   � %               o%   o           %               
"   
 �           �    1� �   � �   � %               o%   o           %              
"   
 � �          h    1� �   � � �     
"   
 �           �    1� �  
 � �   � %               o%   o           %               
"   
 �                1� �   � �   � %               o%   o           � �    
"   
 � �          �    1� �   � �      
"   
 �           �    1� �   � �   � %               o%   o           �   t 
"   
 � �          D	    1� w  
 � �      
"   
 �           �	    1� �   � �   � %               o%   o           � �  � 
"   
 �           �	    1�     � �   � %               o%   o           � �    
"   
 �           h
    1� 7  
 � B   � %               o%   o           %               
"   
 �           �
    1� F   � �   � %               o%   o           %              
"   
 �           `    1� N   � �   � %               o%   o           � �    
"   
 �           �    1� _   � �   � %               o%   o           o%   o           
"   
 �           P    1� o  
 � �   � %               o%   o           � �    
"   
 �           �    1� z   � �  	 � %               o%   o           � �  / 
"   
 � �          8    1� �   � � �  	   
"   
 �           t    1� �   � �  	 � o%   o           o%   o           � �    
"   
 � �          �    1� �   � � �  	   
"   
 �           $    1� �   � �  	 � o%   o           o%   o           � �    
"   
 � �          �    1� 	   � � �     
"   
 � �          �    1�    � � �  	   
"   
 � �              1� $   � � �  	   
"   
 � �          L    1� 1   � � �  	   
"   
 �           �    1� ?   � �   � o%   o           o%   o           %              
"   
 � �              1� P   � � �  	   
"   
 � �          @    1� ^  
 � � i     
"   
 � �          |    1� q   � � �  	   
"   
 � �          �    1� �   � � �  	   
"   
 � �          �    1� �   � � �  	   
"   
 � �          0    1� �   � � �  	   
"   
 � �          l    1� �  	 � � �  	   
"   
 � �          �    1� �   � � �  	   
"   
 � �          �    1� �   � � �  	   
"   
 �                1� �   � �   � %               o%   o           o%   o           
�H T   %              �     }        �GG %              
"   
   
"   
 
"   
   
"   
 �(�  L ( l       �        �    �� �   � P   �        �    �@    
� @  , 
�            ��       p�               �L
�    %              � 8          � $         �           
�    � !     
"   
 �� @  , 
�           �� �  
 �p�               �L"      P �L 
�H T   %              �     }        �GG %              
"   
 �           �    1� $  
 � �   � %               o%   o           � �    
"   
 �           <    1� /  
 � �   � %               o%   o           o%   o           
"   
 �           �    1� :   �    � %               o%   o           o%   o           
"   
 �           4    1� C   � �   � %               o%   o           %               
"   
 �           �    1� R   � �   � %               o%   o           %               
"   
 n�           ,    1� _   n� �   � %               o%   o           � �    
"   
 �           �    1� f   � �   � %               o%   o           %              
"   
 �               1� x   � �   � %               o%   o           o%   o           
"   
 �           �    1� �   � �   � %               o%   o           o%   o           
"   
 �               1� �  	 � �   � %               o%   o           � �    
"   
 �           �    1� �   � �   � %               o%   o           o%   o           
"   
 �               1� �   � �   � %               o%   o           o%   o           
"   
 �           �    1� �   � �   � %               o%   o           %               
"   
 �           �    1� �   � �   � %               o%   o           o%   o           P �L 
�H T   %              �     }        �GG %              
"   
 �           �    1� �  
 � �   � %               o%   o           %              
"   
 �           H    1� �   � �   � %               o%   o           o%   o           
"   
 �           �    1� �   � �   � %               o%   o           � �    
"   
 �           8    1�     � �   � %               o%   o           o%   o           
"   
 � �          �    1�    � �      
"   
 �           �    1�    � �   � %               o%   o           � ,  ! 
"   
 �           d    1� N   � �   � %               o%   o           � �    
"   
 �           �    1� [   � �   � %               o%   o           � n   
"   
 � �          L    1� }   � � �     
"   
 � �          �    1� �   � �      
"   
 �           �    1� �   � �   � %               o%   o           � �    
"   
 � �          8     1� �  
 � �      
"   
 n�           t     1� �   n� �   � %               o%   o           o%   o           
"   
 �           �     1� �   � �   � %               o%   o           %               
"   
 �           l!    1� �   � �   � %               o%   o           %               
"   
 �           �!    1� �   � �   � %               o%   o           � �    
"   
 �           \"    1� �   � �   � %               o%   o           o%   o           
"   
 �           �"    1�    � �   � %               o%   o           %              
"   
 �           T#    1�    � �   � %               o%   o           %               
"   
 n�           �#    1� !   n� �   � %               o%   o           %               
"   
 � �          L$    1� 1   � �      
"   
 � �          �$    1� >   � � �     
"   
 �           �$    1� K   � B   � %               o%   o           o%   o           
"   
 �           @%    1� W   � �   � %               o%   o           � �    
"   
 �           �%    1� e   � �   � %               o%   o           o%   o           
"   
 �           0&    1� s   � �   � o%   o           o%   o           o%   o           
"   
 �           �&    1� �   � �  	 � %               o%   o           o%   o           
"   
 �           ('    1� �   � �   � %               o%   o           o%   o           
"   
 �           �'    1� �  
 � B   � %               o%   o           o%   o           
"   
 � �           (    1� �   � � �     
"   
 �           \(    1� �   � �   � %               o%   o           � �  4 
"   
 �           �(    1�   
 � �   � %               o%   o           %              
"   
 � �          L)    1�    � �      
"   
 �           �)    1� *   � �   � %               o%   o           � �    n
"   
 �           �)    1� 8   � �   � %               o%   o           %              
"   
 �           x*    1� G   � �   � %               o%   o           � �    
"   
 �           �*    1� T   � �   � %               o%   o           � �    
"   
 �           `+    1� b   � �   � %               o%   o           � �    
"   
 �           �+    1� n   � �   � %               o%   o           %               
"   
 �           P,    1� }  	 �    � %               o%   o           o%   o           
"   
 n�           �,    1� �   n� �   � %               o%   o           � �  	 
"   
 �           @-    1� �   � B   � %               o%   o           %       �       
"   
 �           �-    1� �   � �   � %               o%   o           � �    
"   
 �           0.    1� �   � �   � o%   o           o%   o           %              
"   
 �           �.    1� �   � �   � %               o%   o           %               
"   
 �           (/    1� �   � �   � %               o%   o           o%   o           
"   
 �           �/    1� �   � �  	 � %               o%   o           � �    
"   
 � �          0    1� �   � � �  	   P �L 
�H T   %              �     }        �GG %              
"   
 n�           �0    1�   
 n� �   � %               o%   o           � �    n
"   
 �           1    1�    � �   � %               o%   o           %               
"   
 �           �1    1� #  	 � �   � %               o%   o           � �    
"   
 �           2    1� -   � �   � %               o%   o           � �    
"   
 �           �2    1� ;   � �   � %               o%   o           %               
"   
 �           �2    1� K   � �   � %               o%   o           � �    
"   
 �           p3    1� ^   � �   � %               o%   o           o%   o           
"   
 �           �3    1� f   � �   � %               o%   o           o%   o           
"   
 �           h4    1� s   � �   � %               o%   o           o%   o           
"   
 n�           �4    1� �   n� �   � %               o%   o           o%   o           
"   
 �           `5    1� �   � �   � %               o%   o           o%   o           
"   
 �           �5    1� �   � �   � %               o%   o           o%   o           
"   
 �           X6    1� �  	 � �  	 � %               o%   o           � �    
"   
 �           �6    1� �  
 � �  	 � %               o%   o           � �    
"   
 �           @7    1� �   � �   � %               o%   o           � �    
"   
 �           �7    1� �   � �   � %               o%   o           o%   o           
"   
 �           08    1� �   � �   � %               o%   o           o%   o           
"   
 �           �8    1� �   � �   � %               o%   o           � �    
"   
 �            9    1�    � �   � %               o%   o           � �    
"   
 �           �9    1�    � �  	 � %               o%   o           o%   o           
"   
 � �          :    1� &   � �      
"   
 �           L:    1� 2   � �   � %               o%   o           � �    
"   
 �           �:    1� @   � �   � %               o%   o           o%   o           
"   
 n�           <;    1� S   n� �   � %               o%   o           o%   o           
"   
 �           �;    1� e  
 � �   � %               o%   o           � �    
"   
 �           ,<    1� p   � �   � %               o%   o           � �    
"   
 �           �<    1� �   � �   � %               o%   o           %               P �L 
�H T   %              �     }        �GG %              
"   
 �           p=    1� �  	 �    � %               o%   o           o%   o           
"   
 �           �=    1� �   �    � %               o%   o           o%   o           
"   
 �           h>    1� �   �    � %               o%   o           o%   o           
"   
 n�           �>    1� �   n� �   � %               o%   o           %              
"   
 �           `?    1� �   � �   � %               o%   o           � �  M n
"   
 �           �?    1� <   � �   � %               o%   o           %              
"   
 �           P@    1� M   � �   � %               o%   o           %               
"   
 �           �@    1� a   � �   � %               o%   o           %               
"   
 �           HA    1� x   � �  	 � %               o%   o           � �   
"   
 �           �A    1� �   � �   � %               o%   o           %               
"   
 �           8B    1� �   � �  	 � %               o%   o           o%   o           
"   
 �           �B    1� �   � �   � o%   o           o%   o           %              
"   
 n�           0C    1� �   n� �  	 � o%   o           o%   o           � �    n
"   
 �           �C    1� �   �    � o%   o           o%   o           o%   o           
"   
 �            D    1� �   �    � o%   o           o%   o           o%   o           
"   
 �           �D    1� �   � �  	 � o%   o           o%   o           o%   o           
"   
 �           E    1�    �    � o%   o           o%   o           o%   o           
"   
 �           �E    1�    � �  	 � o%   o           o%   o           �    
"   
 �           F    1� !   � �  	 � o%   o           o%   o           � 0   
"   
 �           |F    1� <   � �   � %               o%   o           %               
"   
 �           �F    1� P   � �   � %               o%   o           %               
"   
 � �          tG    1� d   � � �  	   
"   
 �           �G    1� x   � �   � %               o%   o           %               
"   
 �           ,H    1� �   � �   � %               o%   o           o%   o           
"   
 �           �H    1� �   � �   � %               o%   o           o%   o           
"   
 �           $I    1� �   � �   � %               o%   o           o%   o           
"   
 �           �I    1� �   � �   � %               o%   o           � �    
"   
 �           J    1� �   � �   � %               o%   o           %               
"   
 �           �J    1� �  	 � �   � %               o%   o           %                "    � %     start-super-proc �� %     adm2/smart.p d�P �L 
�H T   %              �     }        �GG %              
"   
   �       �K    6� �     
"   
   
�        �K    8
"   
   �        �K    ��     }        �G 4              
"   
 ߱G %              G %              %�   AppService,ASInfo,ASUsePrompt,CacheDuration,CheckCurrentChanged,DestroyStateless,DisconnectAppServer,ServerOperatingMode,ShareData,UpdateFromSource,ForeignFields,ObjectName,OpenOnInit,PromptColumns,PromptOnDelete,RowsToBatch,RebuildOnRepos,ToggleDataTargets  
�H T   %              �     }        �GG %              
"   
 �
"   
 � 
"   
 �
"   
   (�  L ( l       �        �M    �� �   � P   �        �M    �@    
� @  , 
�       �M    ��     �p�               �L
�    %              � 8       N    � $         �           
�    � !   �
"   
 �p� @  , 
�       O    ��    �p�               �L"  	  , �   �    �    � �     }        �A      |    "  	    �    %              (<   \ (    |    �     }        �A�    �A"  
      "  	  �"  
    < "  	  �"  
  (    |    �     }        �A�    �A"  
  
�H T   %              �     }        �GG %              
"   
 �
"   
 � 
"   
 �
"   
   (�  L ( l       �        �P    �� �   � P   �        �P    �@    
� @  , 
�       �P    ��     �p�               �L
�    %              � 8      Q    � $         �           
�    � !   �
"   
 �p� @  , 
�       R    �� u  
 �p�               �L"  	  , 
�H T   %              �     }        �GG %              
"   
   
"   
 �
"   
   
"   
   (�  L ( l       �        �R    �� �   � P   �        �R    �@    
� @  , 
�       �R    ��       p�               �L
�    %              � 8      �R    � $         �           
�    � !     
"   
 �p� @  , 
�       �S    �� �  
 �p�               �L%     SmartDataObject 
"   
   p� @  , 
�       XT    �� �     p�               �L%               
"   
  p� @  , 
�       �T    �� �    p�               �L%               
"   
  p� @  , 
�       U    �� �    p�               �L(        � �      � �      � �      �     }        �A
�H T   %              �     }        �GG %              
"   
  (   � 
"   
 �    �        �U    �� �   �
"   
   � 8      DV    � $         �           
�    � !   �
"   
   �        �V    �
"   
   �       �V    /
"   
   
"   
   �       �V    6� �     
"   
   
�        W    8
"   
   �        4W    �
"   
   �       TW    �
"   
   p�    � F   
�    �     }        �G 4              
"   
 ߱G %              G %              
�     }        �
"   
    (   � 
"   
 �    �        X    �A"    �A
"   
   
�        dX    �@ � 
"   
 "      �       }        �
"   
 � %              %                "    � %     start-super-proc �� %     adm2/appserver.p >�    � �     
�    �     }        �%               %      Server  - �     }        �    "    � �    � %               %      Client      "    � �    � %      NONE    p�,  8         $     "            � �   �
�    
�H T   %              �     }        �GG %              
"   
 �
"   
 � 
"   
 �
"   
   (�  L ( l       �        �Z    �� �   � P   �        �Z    �@    
� @  , 
�       �Z    ��     �p�               �L
�    %              � 8      �Z    � $         �           
�    � !   �
"   
 �p� @  , 
�       �[    �� �   �p�               �L"    , p�,  8         $     "            � �   �
�     "    � %     start-super-proc �� %     adm2/dataquery.p 
�H T   %              �     }        �GG %              
"   
 �
"   
 � 
"   
 �
"   
 �(�  L ( l       �         ]    �� �   � P   �        ,]    �@    
� @  , 
�       8]    ��     �p�               �L
�    %              � 8      D]    � $         �    �     
�    � !   �
"   
 �p� @  , 
�       T^    �� �   �p�               �L%H > 8   dataAvailable,confirmContinue,isUpdatePending,buildDataRequest  
�H T   %              �     }        �GG %              
"   
 �
"   
 � 
"   
 �
"   
 �(�  L ( l       �        8_    �� �   � P   �        D_    �@    
� @  , 
�       P_    ��     �p�               �L
�    %              � 8      \_    � $         �    �     
�    � !   �
"   
 �p� @  , 
�       l`    �� �   �p�               �L%               "    � %     start-super-proc �� %     adm2/query.p c�%     start-super-proc �� %     adm2/queryext.p % 	    initProps �
�    %4 + $   FOR EACH Almacen NO-LOCK INDEXED-REPOSITION �   � c     � e     � g         "    � o    � 
�H T   %              �     }        �GG %              
"   
 �
"   
 � 
"   
 �
"   
   (�  L ( l       �        Db    �� �   � P   �        Pb    �@    
� @  , 
�       \b    ��     �p�               �L
�    %              � 8      hb    � $         �           
�    � !   �
"   
 �p� @  , 
�       xc    �� #  	 �p�               �L"    , %               �    "      � e         %              %                   "      %                  "      "      T(        "    �%              "    �� e   � "      �       "    ��    "    ��    � � �      �    ��    "     �     S    "      "    �     "    %                � @    �     t T     P   4       � "      (0       4       "      � �      � �    �� c   T ,  %              T   "    "    � � e     �    �� c   T    �    "    �    � "      �    �"      %                   %              %                   "      %                  "      �     "       \      H   "      ((       "    �%              � �    � � p     4  �     "      
�H T   %              �     }        �GG %              
"   
 �
"   
 � 
"   
 �
"   
   (�  L ( l       �         h    �� �   � P   �        ,h    �@    
� @  , 
�       8h    ��     �p�               �L
�    %              � 8      Dh    � $         �           
�    � !   �
"   
 �p� @  , 
�       Ti    ��   
 �p�               �L"    ,       "  
  ��    � r   � e   �       "  	    �    � r   � � e   �   � c     � e     � r   ��   � c     � e   �� r   �   � c     � e     � r     
�H T   %              �     }        �GG %              
"   
 � 
"   
 �
"   
 � 
"   
 � (�  L ( l       �        �j    �� �   � P   �        �j    �@    
� @  , 
�       �j    ��     � p�               �L
�    %              � 8      k    � $         �           
�    � !     
"   
 �p� @  , 
�       l    �� �   �p�               �L"    , 
"   
   p� @  , 
�       pl    �� p     p�               �L"    , 
"   
  p� @  , 
�       �l    �� K    p�               �L"    ,     %              %                   "      %                  "      �     "      4 (        "  
    �    � r     � e         "  	  ��     "    T    "      "      @ A,    �   � c   � � p     "    �"       T      @   "    � (        "      � �    �� �      � c   �"         "  	   %              D H   @ A,    �   � c   �� p     "    �"    ,    S   "    �� r   � e   � %                T      @   "    � (        "      � �    �� �      � c   �"         "  
   %                         "    � � p     "    �           "      � p   �"      
�H T   %              �     }        �GG %              
"   
 
"   
   
"   
 
"   
 �(�  L ( l       �        �p    �� �   � P   �        �p    �@    
� @  , 
�       �p    ��     p�               �L
�    %              � 8      �p    � $         �    �     
�    � !   � 
"   
 �p� @  , 
�       r    �� p   �p�               �L"    , 
"   
   p� @  , 
�       \r    �� K     p�               �L"    , "      %              %              %              %              %              %              %              %              %       	       %       
       %              %              %              %              %              %              %              %              "       "    � %     start-super-proc �� %     adm2/data.p %     start-super-proc �� %     adm2/dataext.p %     start-super-proc �� %     adm2/dataextcols.p %     start-super-proc �� %     adm2/dataextapi.p 
�H T   %              �     }        �GG %              
"   
 �
"   
 � 
"   
 �
"   
 �(�  L ( l       �        �u    �� �   � P   �        �u    �@    
� @  , 
�       �u    ��     �p�               �L
�    %              � 8      �u    � $         �    �     
�    � !   �
"   
 �p� @  , 
�       �v    �� x   �p�               �L%               %     "q-almcen.i"    
�H T   %              �     }        �GG %              
"   
 �
"   
 � 
"   
 �
"   
   (�  L ( l       �        �w    �� �   � P   �        �w    �@    
� @  , 
�       �w    ��     �p�               �L
�    %              � 8      �w    � $         �           
�    � !   �
"   
 �p� @  , 
�       �x    �� x   �p�               �L"    , 
�     	         �G
�H T   %              �     }        �GG %              
"   
 �
"   
 � 
"   
 �
"   
   (�  L ( l       �        ly    �� �   � P   �        xy    �@    
� @  , 
�       �y    ��     �p�               �L
�    %              � 8      �y    � $         �           
�    � !   �
"   
 �p� @  , 
�       �z    �� �  
 �p�               �L
"   
 , 
�     
        �G
�H T   %              �     }        �GG %              
"   
 �
"   
 � 
"   
 �
"   
   (�  L ( l       �        X{    �� �   � P   �        d{    �@    
� @  , 
�       p{    ��     �p�               �L
�    %              � 8      |{    � $         �           
�    � !   �
"   
 �p� @  , 
�       �|    �� }  	 �p�               �L
"   
 , 
"   
 �      � �  	   �        �|    �
�H T   %              �     }        �GG %              
"   
 �
"   
 � 
"   
 �
"   
   (�  L ( l       �        d}    �� �   � P   �        p}    �@    
� @  , 
�       |}    ��     �p�               �L
�    %              � 8      �}    � $         �           
�    � !   �
"   
 �p� @  , 
�       �~    �� �   �p�               �L"    , 
"   
   �       �~    �"      
�     
        �G
�H T   %              �     }        �GG %              
"   
 �
"   
 � 
"   
 �
"   
   (�  L ( l       �        |    �� �   � P   �        �    �@    
� @  , 
�       �    ��     �p�               �L
�    %              � 8      �    � $         �           
�    � !   �
"   
 �p� @  , 
�       ��    �� �  	 �p�               �L
"   
 , 
�             �Gp�,  8         $     
"   
 n        � �   �
�    
�             �Gp�,  8         $     
"   
 n        � �   �
�    �    � �     
�        "    � �    � %     modifyListProperty  
�    %      REMOVE  %     SupportedLinks %     Update-Target  %     bufferValidate  
�    "      �  %      setContextAndInitialize 
�    "      %     bufferCommit    
�    "      "      �    � .     
�    %               %     bufferCommit    
�    "      "      
�     
        �G�     }        �
�                    �           x   `       ��                 ^  �  �               ,��        O   ����    e�          O   ����    R�          O   ����    ��         $   m  �   ���                       �U     
                    � ߱               n    �          4   ����V                �                      ��                  o  �                  ���           o  (  �  �  p  PV             r  �  4          4   �����V                D                      ��                  s  �                  ���           s  �  x  o   t      ,                                 �  �   u  �V      �  �   v  �V      �  $   w  �  ���                        W     
                    � ߱          �   x  @W         �   y  `W      4  �   |  �W          $     `  ���                       �W  @         �W              � ߱                     (                T �            
             
             
             
                 $   4   D          $   4   D   ����        ��                            ����                                            �           x   `       ��                 �  �  �               0��        O   ����    e�          O   ����    R�          O   ����    ��      g                      �          �  $   �  �   ���                       X     
                    � ߱                  �  �                      ��                   �  �                  ��          �  (      4   ����$X      $   �  �  ���                       pX     
                    � ߱        d     �    (          4   �����X      /  �  T                               3   �����X  x  �   �  �X          O   �  ��  ��  �X               �          �  �   , �                          
                               �      ��                            ����                                            �           x   `       ��                 M  �  �               ���        O   ����    e�          O   ����    R�          O   ����    ��         $   m  �   ���                       ta                         � ߱        X  $   n  ,  ���                       �a                         � ߱             �  p  �          4   �����a  �a     
                tb                     �c  @        
 �c              � ߱            V   �  �  ���                        h  $   �  <  ���                       �c                         � ߱           $   �  �  ���                       �c                         � ߱          0      �  �                      ��        0          �                    �A�    8     �  �      $   �  \  ���                       d                         � ߱        �  $   �  �  ���                       4d                         � ߱            4   ����\d  |d                     �d                     �d                      e                     @e                         � ߱        �  $   �  �  ���                                �  �          4   ����`e      $       ���                       �e          �f             � ߱        �  $     d  ���                       �f                         � ߱                 X  �                      ��        0                              �E�    |       �      $     ,  ���                       �f                         � ߱        �  $     �  ���                       g                         � ߱            4   ����,g      $     �  ���                       Tg                         � ߱        �g     
                Ph                     �i  @        
 `i              � ߱        (  V   #    ���                        �i       
       
       �i       	       	       j                     @j                         � ߱        T  $   j  �  ���                       H	  $     �  ���                       lj                         � ߱        �j     
                k                     dl  @        
 $l          �l  @        
 |l          m  @        
 �l              � ߱        �	  V     �  ���                          �	      <
  �
                      ��        0          ~  �                  D�k    l     ~  t	      $   ~  
  ���                        m                         � ߱        �
  $   ~  h
  ���                       Pm                         � ߱        �
  4   ����xm      4   �����m    $   �  �
  ���                       n                         � ߱             �  $  �          4   ����$n                �                      ��                  �  �                  Аk           �  4  hn                     �n       	       	           � ߱            $   �  �  ���                              �  0  �          4   �����n                �                      ��                  �  �                  d�k           �  @  �o                     �o       
       
           � ߱            $   �  �  ���                       p                     Pp                         � ߱          $   �  $  ���                       �p     
                 q                     Pr  @        
 r          �r  @        
 hr              � ߱            V   �  �  ���                                    7            �  |  � x                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             �   �   �   �   �   �   �   �       $  4  D  T  d  t  �  \  �  �  �  �  8HXhx��������(8HX      �   �   �   �   �   �   �   �      $  4  D  T  d  t  �   \ �  �  �  �  8HXhx��������(8HX  �                    � �                     �          ��                            ����                                                        x   `       ��                  T  _  �               4�k        O   ����    e�          O   ����    R�          O   ����    ��              !                    ��                            ����                                            �           x   `       ��                  i  x  �               ��k        O   ����    e�          O   ����    R�          O   ����    ��      �       �              �                  $                  X  /  u       (  |�                      3   ����`�            H                      3   ������      O   v  ��  ��  ��               �          �  �    �                                             ��                            ����                                            <          x   `       ��                  �  �  �               ���        O   ����    e�          O   ����    R�          O   ����    ��             �              �          �       $                                       �          $                               �  /  �  h     x  ��                      3   ������            �                      3   ������  �  /  �  �     �  �                      3   ����̂  l                            3   ������      $   �  @  ���                                                   � ߱                  �  �                  3   ������      $   �  �  ���                                                   � ߱        L  $   �     ���                       �                         � ߱            O   �  ��  ��  $�               �          �  �   @ �                                                              0              0           ��                            ����                                                      x   `       ��                  �  �  �               4�        O   ����    e�          O   ����    R�          O   ����    ��      �       $                         �              �          $                      �              /  �  @     P  T�                      3   ����8�  �        p  �                  3   ����\�      $   �  �  ���                                                   � ߱                  �                    3   ����h�      $   �  4  ���                                                   � ߱                     �          �  �   , �                                                                 ��                            ����                                            �           x   `       ��                  -  8  �               �j        O   ����    e�          O   ����    R�          O   ����    ��             7  �   �           4   ������      �   7  ��    ��                            ����                            TXS appSrvUtils E:\OpenEdge\on_in_co\APLIC\q-almcen.w should only be RUN PERSISTENT. ADDROW CANCELROW CANNAVIGATE CLOSEQUERY COLUMNPROPS COLVALUES COPYROW CREATEROW DELETEROW FETCHROW FETCHROWIDENT FINDROW FINDROWWHERE FIRSTROWIDS GETLASTCOMMITERRORTYPE HASFOREIGNKEYCHANGED OPENDATAQUERY OPENQUERY PREPAREQUERY ROWAVAILABLE ROWVALUES SUBMITROW UPDATEROW GETOBJECTTYPE xiRocketIndexLimit ADDQUERYWHERE ASSIGNQUERYSELECTION BUFFERCOMPAREDBTORO BUFFERWHERECLAUSE COLUMNDATATYPE COLUMNDBCOLUMN COLUMNQUERYSELECTION COLUMNTABLE COLUMNVALMSG DBCOLUMNDATANAME DBCOLUMNHANDLE EXCLUDECOLUMNS GETDATACOLUMNS GETFOREIGNVALUES GETQUERYPOSITION GETQUERYSORT GETQUERYSTRING GETQUERYWHERE GETTARGETPROCEDURE INDEXINFORMATION INSERTEXPRESSION NEWQUERYSTRING NEWQUERYVALIDATE NEWQUERYWHERE NEWWHERECLAUSE REFRESHROWIDENT REMOVEFOREIGNKEY REMOVEQUERYSELECTION RESOLVEBUFFER ROWIDWHERE ROWIDWHERECOLS SETQUERYPOSITION SETQUERYSORT SETQUERYSTRING SETQUERYWHERE WHERECLAUSEBUFFER GETAPPSERVICE GETASBOUND GETASDIVISION GETASHANDLE GETASHASSTARTED GETASINFO GETASINITIALIZEONRUN GETASUSEPROMPT GETSERVERFILENAME GETSERVEROPERATINGMODE RUNSERVERPROCEDURE SETAPPSERVICE SETASDIVISION SETASHANDLE SETASINFO SETASINITIALIZEONRUN SETASUSEPROMPT SETSERVERFILENAME SETSERVEROPERATINGMODE gshAstraAppserver gshSessionManager gshRIManager gshSecurityManager gshProfileManager gshRepositoryManager gshTranslationManager gshWebManager gscSessionId gsdSessionObj gshFinManager gshGenManager gshAgnManager gsdTempUniqueID gsdUserObj gsdRenderTypeObj gsdSessionScopeObj ghProp ghADMProps ghADMPropsBuf glADMLoadFromRepos glADMOk ANYMESSAGE ASSIGNLINKPROPERTY FETCHMESSAGES GETCHILDDATAKEY GETCONTAINERHANDLE GETCONTAINERHIDDEN GETCONTAINERSOURCE GETCONTAINERSOURCEEVENTS GETCONTAINERTYPE GETDATALINKSENABLED GETDATASOURCE GETDATASOURCEEVENTS GETDATASOURCENAMES GETDATATARGET GETDATATARGETEVENTS GETDBAWARE GETDESIGNDATAOBJECT GETDYNAMICOBJECT GETINSTANCEPROPERTIES GETLOGICALOBJECTNAME GETLOGICALVERSION GETOBJECTHIDDEN GETOBJECTINITIALIZED GETOBJECTNAME GETOBJECTPAGE GETOBJECTPARENT GETOBJECTVERSION GETOBJECTVERSIONNUMBER GETPARENTDATAKEY GETPASSTHROUGHLINKS GETPHYSICALOBJECTNAME GETPHYSICALVERSION GETPROPERTYDIALOG GETQUERYOBJECT GETRUNATTRIBUTE GETSUPPORTEDLINKS GETTRANSLATABLEPROPERTIES GETUIBMODE GETUSERPROPERTY INSTANCEPROPERTYLIST LINKHANDLES LINKPROPERTY MAPPEDENTRY MESSAGENUMBER PROPERTYTYPE REVIEWMESSAGES SETCHILDDATAKEY SETCONTAINERHIDDEN SETCONTAINERSOURCE SETCONTAINERSOURCEEVENTS SETDATALINKSENABLED SETDATASOURCE SETDATASOURCEEVENTS SETDATASOURCENAMES SETDATATARGET SETDATATARGETEVENTS SETDBAWARE SETDESIGNDATAOBJECT SETDYNAMICOBJECT SETINSTANCEPROPERTIES SETLOGICALOBJECTNAME SETLOGICALVERSION SETOBJECTNAME SETOBJECTPARENT SETOBJECTVERSION SETPARENTDATAKEY SETPASSTHROUGHLINKS SETPHYSICALOBJECTNAME SETPHYSICALVERSION SETRUNATTRIBUTE SETSUPPORTEDLINKS SETTRANSLATABLEPROPERTIES SETUIBMODE SETUSERPROPERTY SHOWMESSAGE SIGNATURE prepareInstance ObjectName CHAR  ObjectVersion ADM2.2 ObjectType SmartDataObject ContainerType PropertyDialog adm2/support/datad.w QueryObject LOGICAL ContainerHandle HANDLE InstanceProperties AppService,ASInfo,ASUsePrompt,CacheDuration,CheckCurrentChanged,DestroyStateless,DisconnectAppServer,ServerOperatingMode,ShareData,UpdateFromSource,ForeignFields,ObjectName,OpenOnInit,PromptColumns,PromptOnDelete,RowsToBatch,RebuildOnRepos,ToggleDataTargets SupportedLinks Data-Source,Data-Target,Navigation-Target,Update-Target,Commit-Target,Filter-Target ContainerHidden ObjectInitialized ObjectHidden ObjectsCreated HideOnInit UIBMode ContainerSource ContainerSourceEvents initializeObject,hideObject,viewObject,destroyObject,enableObject,confirmExit,confirmCancel,confirmOk,isUpdateActive DataSource DataSourceEvents dataAvailable,queryPosition,updateState,deleteComplete,fetchDataSet,confirmContinue,confirmCommit,confirmUndo,assignMaxGuess,isUpdatePending TranslatableProperties ObjectPage INT DBAware DesignDataObject DataSourceNames DataTarget DataTargetEvents CHARACTER updateState,rowObjectState,fetchBatch,LinkState LogicalObjectName PhysicalObjectName LogicalVersion PhysicalVersion DynamicObject RunAttribute ChildDataKey ParentDataKey DataLinksEnabled InactiveLinks InstanceId DECIMAL SuperProcedure SuperProcedureMode SuperProcedureHandle LayoutPosition ClassName RenderingProcedure ThinRenderingProcedure Label cType ADMProps Target WHERE Target = WIDGET-H(" ") AppService ASDivision ASHandle ASHasConnected ASHasStarted ASInfo ASInitializeOnRun ASUsePrompt BindSignature BindScope ServerOperatingMode ServerFileName ServerFirstCall NeedContext AutoCommit BLOBColumns BufferHandles CLOBColumns CommitSource CommitSourceEvents commitTransaction,undoTransaction CommitTarget CommitTargetEvents rowObjectState CurrentRowid ROWID CurrentUpdateSource DataColumns DataHandle DataIsFetched DataModified DataQueryBrowsed DataQueryString FetchOnOpen FillBatchOnRepos FilterActive FilterAvailable FilterSource FilterWindow FirstRowNum ForeignFields ForeignValues IgnoreTreeViewFilter IndexInformation LargeColumns LastRowNum NavigationSource NavigationSourceEvents fetchFirst,fetchNext,fetchPrev,fetchLast,startFilter OpenOnInit PrimarySDOSource PromptColumns PromptOnDelete QueryColumns QueryPosition QueryString RebuildOnRepos RowObject RowObjectState NoUpdates RowsToBatch Tables ToggleDataTargets TransferChildrenForAll UpdatableColumns UpdatableWhenNew UpdateSource AssignList AuditEnabled BaseQuery CalcFieldList CheckLastOnOpen DataColumnsByTable DBNames EntityFields FetchHasAudit FetchHasComment FetchAutoComment FirstResultRow KeyFields KeyTableId LastDBRowIdent LastResultRow NewBatchInfo NoLockReadOnlyTables PhysicalTables PositionForClient QueryHandle QueryRowIdent RequiredProperties SkipTransferDBRow TempTables UpdatableColumnsByTable UpdateFromSource RowObjUpd RowObjectTable RowObjUpdTable CheckCurrentChanged StatelessSavedProperties CheckCurrentChanged,RowObjectState,LastResultRow,FirstResultRow,QueryRowIdent DestroyStateless DisconnectAppServer ServerSubmitValidation DataFieldDefs "q-almcen.i" QueryContainer QueryContext AsynchronousSDO DataLogicProcedure DataLogicObject DataReadHandler DataReadColumns DataReadBuffer DataDelimiter | DataReadFormat TrimNumeric IsRowObjectExternal IsRowObjUpdExternal ManualAddQueryWhere DynamicData LastCommitErrorType LastCommitErrorKeys RunDataLogicProxy SchemaLocation CacheDuration INTEGER ShareData ghContainer adm2/smart.p cObjectName iStart / \ . hReposBuffer hPropTable hBuffer hTable deleteProperties ADM-CLONE-PROPS pcProcName hProc START-SUPER-PROC cAppService cASDivision cServerOperatingMode adm2/appserver.p getAppService Server Client NONE setASDivision setAppService adm2/dataquery.p dataAvailable,confirmContinue,isUpdatePending,buildDataRequest adm2/query.p adm2/queryext.p cTable iTable cColumns cDataCols cUpdCols cCalcData cCalcUpd iNumData iNumUpd cBuffers cKeyFields cAssignList iAssigns iPos iEntry iCount cTables cTableAssign cDbEntry cField cKeyTable cQueryString FOR EACH Almacen NO-LOCK INDEXED-REPOSITION ,   Almacen  ; CodCia CodAlm Descripcion INITPROPS piTableIndex lRet DELETERECORDSTATIC adm2/data.p adm2/dataext.p adm2/dataextcols.p adm2/dataextapi.p CodCia CodAlm Descripcion RowNum RowIdent RowMod RowIdentIdx RowUserProp ChangedFields cContainerType hRowObject hDataQuery cDataFieldDefs FOR EACH  setRowObjectTable setRowObjUpdTable getUpdatableColumns REMOVE Update-Target PUSHROWOBJUPDTABLE pcValType PUSHTABLEANDVALIDATE pcContext pcMessages pcUndoIds obtainContextForClient REMOTECOMMIT SERVERCOMMIT GETROWOBJUPDSTATIC DISABLE_UI qDataQuery t  X-  �  �:      / �<   ��      0         pcServices      ��      T         pcServices  �   ��      x         piStartRow  �   ��      �         piStartRow  �   ��      �         piStartRow  �   ��      �         piStartRow      ��              piStartRow  <  ��      ,        pcViewColList       ��      T       
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
 hTarget �  ��      �        pcMessage       ��      �        pcMessage       ��      �        plEnabled             �     cType       0     V   �                             getObjectType   P	  h	  j	  `        P  
   hReposBuffer    �        t  
   hPropTable  �        �  
   hBuffer           �  
   hTable  �  �     W   <          �                  adm-clone-props m  n  o  p  r  s  t  u  v  w  x  y  |    �  �  �            P  
   hProc             p        pcProcName  �  �  	   X   <  X      �                  start-super-proc    �  �  �  �  �  �  �  �  �           �     cTable               iTable  <        0     cColumns    \        P     cDataCols   |        p     cUpdCols    �        �     cCalcData   �        �     cCalcUpd    �     	   �     iNumData    �     
   �     iNumUpd              cBuffers    8        ,     cKeyFields  X        L     cAssignList x        l     iAssigns    �        �     iPos    �        �     iEntry  �        �     iCount  �        �     cTables        �     cTableAssign    ,              cDbEntry    H        @     cField  h        \     cKeyTable             |     cQueryString    |  �  !   Y   �          �                  initProps   m  n  �  �  �  �  �  �                #  j      ~  �  �  �  �  �  �  �  �  �  �  �  �  �            `     lRet              �        piTableIndex    �  �  (   Z   L  h      �                  deleteRecordStatic          ;  <  X  Y  u  v  �  �  �  �  �  �  �  �      #  $  @  A  ]  ^  z  {  �  �  �  �  �  �  �  �                         !       �  �     [       t      �                  pushRowObjUpdTable  _  �        �        pcValType                  $       �  X     \       �      @                  pushTableAndValidate    u  v  x  �        |        pcContext   �             $       �        �        pcMessages            �        pcUndoIds     (     ]       d                        remoteCommit    �  �  �  �  �  T             $       x        l        pcMessages            �        pcUndoIds   �  �     ^       <      �                  serverCommit    �  �  �  (     _                                 getRowObjUpdStatic  �  �  �  l     `               `                  disable_UI  7  8  0  �       X      `                      p  �  �     RowObject             (         0         <         D         P         X         d         CodCia  CodAlm  Descripcion RowNum  RowIdent    RowMod  RowIdentIdx RowUserProp     �  �  	   RowObjUpd   �                                              (         0         <         H         CodCia  CodAlm  Descripcion RowNum  RowIdent    RowMod  RowIdentIdx RowUserProp ChangedFields   x          l  
   appSrvUtils �       �     xiRocketIndexLimit  �        �  
   gshAstraAppserver   �        �  
   gshSessionManager             
   gshRIManager    <        (  
   gshSecurityManager  d        P  
   gshProfileManager   �        x  
   gshRepositoryManager    �  	 	     �  
   gshTranslationManager   �  
 
     �  
   gshWebManager           �     gscSessionId    (             gsdSessionObj   L        <  
   gshFinManager   p        `  
   gshGenManager   �        �  
   gshAgnManager   �        �     gsdTempUniqueID �        �     gsdUserObj           �     gsdRenderTypeObj    (             gsdSessionScopeObj  D       <  
   ghProp  d       X  
   ghADMProps  �       x  
   ghADMPropsBuf   �       �     glADMLoadFromRepos  �       �     glADMOk �       �  
   ghContainer     	         cObjectName (    
         iStart  H       <     cAppService h       \     cASDivision �       |     cServerOperatingMode    �       �     cContainerType  �       �     cQueryString    �       �  
   hRowObject           
   hDataQuery  <       0     cColumns             P     cDataFieldDefs  |    X  p  RowObject         X  �  RowObjUpd          "   >   �   �   �   �          !  8  D  E  F  H  J  K  L  P  Q  T  U  V  W  Y  [  ]  _  `  a  d  f  g  i  j  k  l  m  s  u  {  }    �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  
  A
  B
  D
  E
  F
  G
  H
  I
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
  _
  `
  a
  b
  c
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
  s
  t
  u
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
                     	  
        ~  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  -  �  �  �  �  �  �  �  �  �      0  O  Q  f  �      !  1  2  3  6  7  8  <  ?  @  ]  q  �  #  $  0  T  �  �  �  �  �  A  $  %  &  '  ,  2  9  �  �  �  �  �  �  �      1  2  <  V  p  z  �  �  �  �      ��  E:\OpenEdge\on_in_co\APLIC\q-almcen.w    �"  ��  C:\Progress\OpenEdge\src\adm2\data.i �"  �) . %C:\Progress\OpenEdge\src\adm2\custom\datacustom.i    (#  �� - C:\Progress\OpenEdge\src\adm2\robjflds.i h#  �h , E:\OpenEdge\on_in_co\APLIC\q-almcen.i    �#  �   C:\Progress\OpenEdge\src\adm2\query.i    �#  z + C:\Progress\OpenEdge\src\adm2\delrecst.i $  `W * C:\Progress\OpenEdge\src\adm2\tblprep.i  8$   ) %C:\Progress\OpenEdge\src\adm2\custom\querycustom.i   l$  � ! C:\Progress\OpenEdge\src\adm2\dataquery.i    �$  �Z ( %C:\Progress\OpenEdge\src\adm2\custom\dataquerycustom.i   �$  �< " C:\Progress\OpenEdge\src\adm2\appserver.i    (%  �� ' %C:\Progress\OpenEdge\src\adm2\custom\appservercustom.i   `%  I� # C:\Progress\OpenEdge\src\adm2\smart.i    �%  Ds & C:\Progress\OpenEdge\gui\fn  �%  tw % %C:\Progress\OpenEdge\src\adm2\custom\smartcustom.i    &  Q. $ C:\Progress\OpenEdge\gui\set @&  �>  C:\Progress\OpenEdge\src\adm2\dataprop.i h&  ��  %C:\Progress\OpenEdge\src\adm2\custom\datapropcustom.i    �&  ��  %C:\Progress\OpenEdge\src\adm2\custom\dataprtocustom.i    �&  �K  C:\Progress\OpenEdge\src\adm2\qryprop.i  $'  -�  %C:\Progress\OpenEdge\src\adm2\custom\qrypropcustom.i X'  ��  %C:\Progress\OpenEdge\src\adm2\custom\qryprtocustom.i �'   
 C:\Progress\OpenEdge\src\adm2\dataqueryprop.i    �'  �d  %C:\Progress\OpenEdge\src\adm2\custom\dataquerypropcustom.i   (  ��  %C:\Progress\OpenEdge\src\adm2\custom\dataqueryprtocustom.i   \(  �l  C:\Progress\OpenEdge\src\adm2\appsprop.i �(  ɏ  %C:\Progress\OpenEdge\src\adm2\custom\appspropcustom.i    �(  V  %C:\Progress\OpenEdge\src\adm2\custom\appsprtocustom.i    )  i$  C:\Progress\OpenEdge\src\adm2\smrtprop.i `)  �j  C:\Progress\OpenEdge\gui\get �)  �  %C:\Progress\OpenEdge\src\adm2\custom\smrtpropcustom.i    �)  ��  %C:\Progress\OpenEdge\src\adm2\custom\smrtprtocustom.i     *  ��  C:\Progress\OpenEdge\src\adm2\smrtprto.i D*  Su  C:\Progress\OpenEdge\src\adm2\globals.i  x*  M�  %C:\Progress\OpenEdge\src\adm2\custom\globalscustom.i �*  )a  %C:\Progress\OpenEdge\src\adm2\custom\smartdefscustom.i   �*  �  C:\Progress\OpenEdge\src\adm2\appsprto.i 0+  ��  %C:\Progress\OpenEdge\src\adm2\custom\appserverdefscustom.i   d+  ��  C:\Progress\OpenEdge\src\adm2\dataqueryprto.i    �+  ª  %C:\Progress\OpenEdge\src\adm2\custom\dataquerydefscustom.i   �+  �� 	 C:\Progress\OpenEdge\src\adm2\qryprto.i  0,  �  %C:\Progress\OpenEdge\src\adm2\custom\querydefscustom.i   d,  �`  C:\Progress\OpenEdge\src\adm2\dataprto.i �,  �  %C:\Progress\OpenEdge\src\adm2\custom\datadefscustom.i    �,  e�  !C:\Progress\OpenEdge\gui\adecomm\appserv.i    -  �i   E:\OpenEdge\on_in_co\APLIC\q-almcen_cl.w        >      �-  �   �     �-  [  �     �-     �  &   �-  �         �-     �  .   �-  �   �     �-     �     �-  �   �     .     |  $   .  �   z     ,.     X  $   <.  �   V     L.     4  $   \.  �   1     l.       $   |.  �        �.     �  $   �.  �   �     �.     �  $   �.  �   �     �.     �  $   �.  �   �     �.     ~  $   �.  �   q     /     Y  -   /  �   U     ,/     Q  ,   </  k        L/  �        \/     �  +   l/  �  �      |/     �  +   �/  �  �      �/     �  +   �/  �  �      �/     �  +   �/  �  �      �/     }  +   �/  �  z      �/     `  +   0  �  ]      0     C  +   ,0  �  @      <0     &  +   L0  �  #      \0     	  +   l0  �        |0     �  +   �0  �  �      �0     �  +   �0  �  �      �0     �  +   �0  �  �      �0     �  +   �0  �  �      �0     x  +   1  �  u      1     [  +   ,1  �  X      <1     >  +   L1  �  ;      \1     !  +   l1  �        |1       +   �1  �  �      �1     �  $   �1  �  �      �1     �  $   �1  j  z      �1     X  $   �1  i  W      �1     5  $   2  h  4      2       $   ,2  ^        <2     �  *   L2  ]  �      \2     �  *   l2  \  �      |2     �  *   �2  [  �      �2     m  *   �2  Z  l      �2     F  *   �2  Y  E      �2       *   �2  X        �2     �  *   3  W  �      3     �  *   ,3  V  �      <3     �  *   L3  U  �      \3     �  *   l3  T  �      |3     \  *   �3  S  [      �3     5  *   �3  R  4      �3       *   �3  Q        �3     �  *   �3  P  �      �3     �  *   4  O  �      4     �  *   ,4  N  �      <4     r  *   L4  M  q      \4     K  *   l4  ?  =      |4       $   �4    �      �4     �  $   �4  �   =      �4     �  )   �4  g   �      �4  a   �  !   �4     p  (   �4  _   n  !   5     L  $   5  ]   J  !   ,5     (  $   <5  I     !   L5  �     "   \5     �  '   l5  �   �  "   |5     �  $   �5  �   �  "   �5     i  $   �5  �   g  "   �5     E  $   �5  g   +  "   �5          �5  O   �  "   �5  �   ~  #   6     |  &   6  �   L  #   ,6     �  %   <6  �   �  #   L6     �  $   \6  �   �  #   l6     �  $   |6  �   �  #   �6     �  $   �6  �   �  #   �6     ^  $   �6  �   J  #   �6     (  $   �6  }     #   �6     �  $   �6     ~  #   7     0  "   7     �  !   ,7     �      <7     6     L7  �   -     \7  O        l7          |7     �     �7  �   �     �7  �   ~     �7  O   p     �7     _     �7          �7  y   �
     �7  �   �
  
   �7  G   �
     8     �
     8     y
     ,8  c   
  
   <8  x   
     L8  M   �	     \8     �	     l8     �	     |8  a   �	     �8  �  g	     �8     H	     �8  �  	     �8  O   	     �8     �     �8     �     �8  �   �     �8     �     9     �     9  x   �     ,9     �     <9     c     L9     _     \9     K     l9     2     |9  Q   "     �9     �     �9     �     �9     |     �9     b     �9  ]   \  
   �9     R     �9     
  
   �9     �     :     �  
   :  Z   �     ,:     �  	   <:     �     L:     �     \:     �     l:  c   f     |:     D     �:     �      �:     �      �:     �      �:     �      �:     &      �:           �:           