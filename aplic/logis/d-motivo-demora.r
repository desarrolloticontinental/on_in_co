	��V 8�a�4  L �                                              � 34EC010Autf-8 MAIN d:\newsie\on_in_co\APLIC\logis\d-motivo-demora.w,,OUTPUT pFlgEstDet CHARACTER,OUTPUT pGlosa CHARACTER PROCEDURE enable_UI,, PROCEDURE disable_UI,, PROCEDURE adm-create-objects,, PROCEDURE start-super-proc,,INPUT pcProcName CHARACTER PROCEDURE adm-clone-props,, PROCEDURE toggleData,,INPUT plEnabled LOGICAL PROCEDURE showMessageProcedure,,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL PROCEDURE returnFocus,,INPUT hTarget HANDLE PROCEDURE repositionObject,,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL PROCEDURE removeLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE removeAllLinks,, PROCEDURE modifyUserLinks,,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE PROCEDURE modifyListProperty,,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER PROCEDURE hideObject,, PROCEDURE exitObject,, PROCEDURE editInstanceProperties,, PROCEDURE displayLinks,, PROCEDURE createControls,, PROCEDURE changeCursor,,INPUT pcCursor CHARACTER PROCEDURE applyEntry,,INPUT pcField CHARACTER PROCEDURE adjustTabOrder,,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER PROCEDURE addMessage,,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER PROCEDURE addLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE unbindServer,,INPUT pcMode CHARACTER PROCEDURE startServerObject,, PROCEDURE runServerObject,,INPUT phAppService HANDLE PROCEDURE restartServerObject,, PROCEDURE initializeServerObject,, PROCEDURE disconnectObject,, PROCEDURE destroyServerObject,, PROCEDURE bindServer,, PROCEDURE processAction,,INPUT pcAction CHARACTER PROCEDURE enableObject,, PROCEDURE disableObject,, PROCEDURE applyLayout,, PROCEDURE viewPage,,INPUT piPageNum INTEGER PROCEDURE viewObject,, PROCEDURE toolbar,,INPUT pcValue CHARACTER PROCEDURE selectPage,,INPUT piPageNum INTEGER PROCEDURE removePageNTarget,,INPUT phTarget HANDLE,INPUT piPage INTEGER PROCEDURE passThrough,,INPUT pcLinkName CHARACTER,INPUT pcArgument CHARACTER PROCEDURE notifyPage,,INPUT pcProc CHARACTER PROCEDURE initPages,,INPUT pcPageList CHARACTER PROCEDURE initializeVisualContainer,, PROCEDURE initializeObject,, PROCEDURE hidePage,,INPUT piPageNum INTEGER PROCEDURE destroyObject,, PROCEDURE deletePage,,INPUT piPageNum INTEGER PROCEDURE createObjects,, PROCEDURE constructObject,,INPUT pcProcName CHARACTER,INPUT phParent HANDLE,INPUT pcPropList CHARACTER,OUTPUT phObject HANDLE PROCEDURE confirmExit,,INPUT-OUTPUT plCancel LOGICAL PROCEDURE changePage,, PROCEDURE assignPageProperty,,INPUT pcProp CHARACTER,INPUT pcValue CHARACTER FUNCTION Signature,CHARACTER,INPUT pcName CHARACTER FUNCTION showmessage,LOGICAL,INPUT pcMessage CHARACTER FUNCTION setUserProperty,LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION setUIBMode,LOGICAL,INPUT pcMode CHARACTER FUNCTION setTranslatableProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setSupportedLinks,LOGICAL,INPUT pcLinkList CHARACTER FUNCTION setRunAttribute,LOGICAL,INPUT cRunAttribute CHARACTER FUNCTION setPhysicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setPhysicalObjectName,LOGICAL,INPUT cTemp CHARACTER FUNCTION setPassThroughLinks,LOGICAL,INPUT pcLinks CHARACTER FUNCTION setParentDataKey,LOGICAL,INPUT cParentDataKey CHARACTER FUNCTION setObjectVersion,LOGICAL,INPUT cObjectVersion CHARACTER FUNCTION setObjectParent,LOGICAL,INPUT phParent HANDLE FUNCTION setObjectName,LOGICAL,INPUT pcName CHARACTER FUNCTION setLogicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setLogicalObjectName,LOGICAL,INPUT c CHARACTER FUNCTION setInstanceProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setDynamicObject,LOGICAL,INPUT lTemp LOGICAL FUNCTION setDesignDataObject,LOGICAL,INPUT pcDataObject CHARACTER FUNCTION setDBAware,LOGICAL,INPUT lAware LOGICAL FUNCTION setDataTargetEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setDataTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setDataSourceNames,LOGICAL,INPUT pcSourceNames CHARACTER FUNCTION setDataSourceEvents,LOGICAL,INPUT pcEventsList CHARACTER FUNCTION setDataSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDataLinksEnabled,LOGICAL,INPUT lDataLinksEnabled LOGICAL FUNCTION setContainerSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setContainerSource,LOGICAL,INPUT phObject HANDLE FUNCTION setContainerHidden,LOGICAL,INPUT plHidden LOGICAL FUNCTION setChildDataKey,LOGICAL,INPUT cChildDataKey CHARACTER FUNCTION reviewMessages,CHARACTER, FUNCTION propertyType,CHARACTER,INPUT pcPropName CHARACTER FUNCTION messageNumber,CHARACTER,INPUT piMessage INTEGER FUNCTION mappedEntry,CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER FUNCTION linkProperty,CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER FUNCTION linkHandles,CHARACTER,INPUT pcLink CHARACTER FUNCTION instancePropertyList,CHARACTER,INPUT pcPropList CHARACTER FUNCTION getUserProperty,CHARACTER,INPUT pcPropName CHARACTER FUNCTION getUIBMode,CHARACTER, FUNCTION getTranslatableProperties,CHARACTER, FUNCTION getSupportedLinks,CHARACTER, FUNCTION getRunAttribute,CHARACTER, FUNCTION getQueryObject,LOGICAL, FUNCTION getPropertyDialog,CHARACTER, FUNCTION getPhysicalVersion,CHARACTER, FUNCTION getPhysicalObjectName,CHARACTER, FUNCTION getPassThroughLinks,CHARACTER, FUNCTION getParentDataKey,CHARACTER, FUNCTION getObjectVersionNumber,CHARACTER, FUNCTION getObjectVersion,CHARACTER, FUNCTION getObjectParent,HANDLE, FUNCTION getObjectPage,INTEGER, FUNCTION getObjectName,CHARACTER, FUNCTION getObjectInitialized,LOGICAL, FUNCTION getObjectHidden,LOGICAL, FUNCTION getLogicalVersion,CHARACTER, FUNCTION getLogicalObjectName,CHARACTER, FUNCTION getInstanceProperties,CHARACTER, FUNCTION getDynamicObject,LOGICAL, FUNCTION getDesignDataObject,CHARACTER, FUNCTION getDBAware,LOGICAL, FUNCTION getDataTargetEvents,CHARACTER, FUNCTION getDataTarget,CHARACTER, FUNCTION getDataSourceNames,CHARACTER, FUNCTION getDataSourceEvents,CHARACTER, FUNCTION getDataSource,HANDLE, FUNCTION getDataLinksEnabled,LOGICAL, FUNCTION getContainerType,CHARACTER, FUNCTION getContainerSourceEvents,CHARACTER, FUNCTION getContainerSource,HANDLE, FUNCTION getContainerHidden,LOGICAL, FUNCTION getContainerHandle,HANDLE, FUNCTION getChildDataKey,CHARACTER, FUNCTION fetchMessages,CHARACTER, FUNCTION assignLinkProperty,LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION anyMessage,LOGICAL, FUNCTION setServerOperatingMode,LOGICAL,INPUT pcServerOperatingMode CHARACTER FUNCTION setServerFileName,LOGICAL,INPUT pcFileName CHARACTER FUNCTION setASUsePrompt,LOGICAL,INPUT plFlag LOGICAL FUNCTION setASInitializeOnRun,LOGICAL,INPUT plInitialize LOGICAL FUNCTION setASInfo,LOGICAL,INPUT pcInfo CHARACTER FUNCTION setASHandle,LOGICAL,INPUT phASHandle HANDLE FUNCTION setASDivision,LOGICAL,INPUT pcDivision CHARACTER FUNCTION setAppService,LOGICAL,INPUT pcAppService CHARACTER FUNCTION runServerProcedure,HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE FUNCTION getServerOperatingMode,CHARACTER, FUNCTION getServerFileName,CHARACTER, FUNCTION getASUsePrompt,LOGICAL, FUNCTION getASInitializeOnRun,LOGICAL, FUNCTION getASInfo,CHARACTER, FUNCTION getASHasStarted,LOGICAL, FUNCTION getASHandle,HANDLE, FUNCTION getAsDivision,CHARACTER, FUNCTION getASBound,LOGICAL, FUNCTION getAppService,CHARACTER, FUNCTION createUiEvents,LOGICAL, FUNCTION getObjectSecured,LOGICAL, FUNCTION getObjectTranslated,LOGICAL, FUNCTION setResizeVertical,LOGICAL,INPUT plResizeVertical LOGICAL FUNCTION setResizeHorizontal,LOGICAL,INPUT plResizeHorizontal LOGICAL FUNCTION setObjectLayout,LOGICAL,INPUT pcLayout CHARACTER FUNCTION setLayoutOptions,LOGICAL,INPUT pcOptions CHARACTER FUNCTION setHideOnInit,LOGICAL,INPUT plHide LOGICAL FUNCTION setDisableOnInit,LOGICAL,INPUT plDisable LOGICAL FUNCTION setDefaultLayout,LOGICAL,INPUT pcDefault CHARACTER FUNCTION setAllFieldNames,LOGICAL,INPUT pcValue CHARACTER FUNCTION setAllFieldHandles,LOGICAL,INPUT pcValue CHARACTER FUNCTION getResizeVertical,LOGICAL, FUNCTION getResizeHorizontal,LOGICAL, FUNCTION getWidth,DECIMAL, FUNCTION getRow,DECIMAL, FUNCTION getObjectLayout,CHARACTER, FUNCTION getObjectEnabled,LOGICAL, FUNCTION getLayoutVariable,CHARACTER, FUNCTION getLayoutOptions,CHARACTER, FUNCTION getHideOnInit,LOGICAL, FUNCTION getHeight,DECIMAL, FUNCTION getEnabledObjHdls,CHARACTER, FUNCTION getEnabledObjFlds,CHARACTER, FUNCTION getDisableOnInit,LOGICAL, FUNCTION getDefaultLayout,CHARACTER, FUNCTION getCol,DECIMAL, FUNCTION getAllFieldNames,CHARACTER, FUNCTION getAllFieldHandles,CHARACTER, FUNCTION setStatusArea,LOGICAL,INPUT plStatusArea LOGICAL FUNCTION getObjectType,character, FUNCTION setWindowTitleViewer,LOGICAL,INPUT phViewer HANDLE FUNCTION setWaitForObject,LOGICAL,INPUT phObject HANDLE FUNCTION setUpdateTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setUpdateSource,LOGICAL,INPUT pcSource CHARACTER FUNCTION setTopOnly,LOGICAL,INPUT plTopOnly LOGICAL FUNCTION setSdoForeignFields,LOGICAL,INPUT cSdoForeignFields CHARACTER FUNCTION setSavedContainerMode,LOGICAL,INPUT cSavedContainerMode CHARACTER FUNCTION setRunMultiple,LOGICAL,INPUT plMultiple LOGICAL FUNCTION setRunDOOptions,LOGICAL,INPUT pcOptions CHARACTER FUNCTION setRouterTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setReEnableDataLinks,LOGICAL,INPUT cReEnableDataLinks CHARACTER FUNCTION setPrimarySdoTarget,LOGICAL,INPUT hPrimarySdoTarget HANDLE FUNCTION setPageSource,LOGICAL,INPUT phObject HANDLE FUNCTION setPageNTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setOutMessageTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setNavigationTarget,LOGICAL,INPUT cTarget CHARACTER FUNCTION setNavigationSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setNavigationSource,LOGICAL,INPUT pcSource CHARACTER FUNCTION setMultiInstanceSupported,LOGICAL,INPUT lMultiInstanceSupported LOGICAL FUNCTION setMultiInstanceActivated,LOGICAL,INPUT lMultiInstanceActivated LOGICAL FUNCTION setInMessageTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setFilterSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDynamicSDOProcedure,LOGICAL,INPUT pcProc CHARACTER FUNCTION setDisabledAddModeTabs,LOGICAL,INPUT cDisabledAddModeTabs CHARACTER FUNCTION setCurrentPage,LOGICAL,INPUT iPage INTEGER FUNCTION setContainerTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setContainerMode,LOGICAL,INPUT cContainerMode CHARACTER FUNCTION setCallerWindow,LOGICAL,INPUT h HANDLE FUNCTION setCallerProcedure,LOGICAL,INPUT h HANDLE FUNCTION setCallerObject,LOGICAL,INPUT h HANDLE FUNCTION pageNTargets,CHARACTER,INPUT phTarget HANDLE,INPUT piPageNum INTEGER FUNCTION getStatusArea,LOGICAL, FUNCTION getWindowTitleViewer,HANDLE, FUNCTION getWaitForObject,HANDLE, FUNCTION getUpdateTarget,CHARACTER, FUNCTION getUpdateSource,CHARACTER, FUNCTION getTopOnly,LOGICAL, FUNCTION getSdoForeignFields,CHARACTER, FUNCTION getSavedContainerMode,CHARACTER, FUNCTION getRunMultiple,LOGICAL, FUNCTION getRunDOOptions,CHARACTER, FUNCTION getReEnableDataLinks,CHARACTER, FUNCTION getPrimarySdoTarget,HANDLE, FUNCTION getPageSource,HANDLE, FUNCTION getPageNTarget,CHARACTER, FUNCTION getOutMessageTarget,HANDLE, FUNCTION getNavigationTarget,HANDLE, FUNCTION getNavigationSourceEvents,CHARACTER, FUNCTION getNavigationSource,CHARACTER, FUNCTION getMultiInstanceSupported,LOGICAL, FUNCTION getMultiInstanceActivated,LOGICAL, FUNCTION getFilterSource,HANDLE, FUNCTION getDynamicSDOProcedure,CHARACTER, FUNCTION getDisabledAddModeTabs,CHARACTER, FUNCTION getCurrentPage,INTEGER, FUNCTION getContainerTargetEvents,CHARACTER, FUNCTION getContainerTarget,CHARACTER, FUNCTION getContainerMode,CHARACTER, FUNCTION getCallerWindow,HANDLE, FUNCTION getCallerProcedure,HANDLE, FUNCTION enablePagesInFolder,LOGICAL,INPUT pcPageInformation CHARACTER FUNCTION disablePagesInFolder,LOGICAL,INPUT pcPageInformation CHARACTER FUNCTION widgetValueList,CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION widgetValue,CHARACTER,INPUT pcName CHARACTER FUNCTION widgetIsTrue,LOGICAL,INPUT pcName CHARACTER FUNCTION widgetIsModified,LOGICAL,INPUT pcNameList CHARACTER FUNCTION widgetIsFocused,LOGICAL,INPUT pcName CHARACTER FUNCTION widgetIsBlank,LOGICAL,INPUT pcNameList CHARACTER FUNCTION widgetLongcharValue,LONGCHAR,INPUT pcName CHARACTER FUNCTION widgetHandle,HANDLE,INPUT pcName CHARACTER FUNCTION viewWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION toggleWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION resetWidgetValue,LOGICAL,INPUT pcNameList CHARACTER FUNCTION highlightWidget,LOGICAL,INPUT pcNameList CHARACTER,INPUT pcHighlightType CHARACTER FUNCTION hideWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION formattedWidgetValueList,CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION formattedWidgetValue,CHARACTER,INPUT pcName CHARACTER FUNCTION enableWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION enableRadioButton,LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER FUNCTION disableWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION disableRadioButton,LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER FUNCTION clearWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION blankWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION assignWidgetValueList,LOGICAL,INPUT pcNameList CHARACTER,INPUT pcValueList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION assignWidgetValue,LOGICAL,INPUT pcName CHARACTER,INPUT pcValue CHARACTER FUNCTION assignFocusedWidget,LOGICAL,INPUT pcName CHARACTER        P              |�              :g P  ,�              L_              �"    +   �4 �  7   h9 `  8   �< �   >   �= 8  ?   �> �  @           �B T  E �  ? H 2  iSO8859-1                                                                           h    �                                       �              �  ��                �  �    �   ޕ    <�           ��  �         $          �                                             PROGRESS                         <           
    
                    <              �                                                                                                     
  �       �             �         �                      �                      INTEGRAL                         PROGRESS                         l     �  �      �                         �ɺ[            �  �e                              �  �                      �  �  %      TABLACODIGONOMBRENOMANTCODCTA1CODCTA2                                                               �  �	      d  
    
                  P               �                                                                                          �	          
  �  �	        
    
                  �  �             �                                                                                          �	          
  @  �	      �  
    
                  �  p             ,                                                                                          �	          
  �  �	      h  
    
                  T               �                                                                                          �	          
  �  
        
    
                     �             �                                                                                          
          
  D   
      �  
    
                  �  t  	           0                                                                                           
          
  �  5
      l  
    
                  X     
           �                                                                                          5
          
  �  K
        
    
                    �             �                                                                                          K
          
  H	  Y
      �                         �  x	             4	                                                                                          Y
            �	  f
      p	                        \	  $
             �	                                                                                          f
            �
  t
      
  
    
                  
  �
             �
                                                                                          t
          
  L  �
      �
  
    
                  �
  |             8                                                                                          �
          
  �  �
      t  
    
                  `  (             �                                                                                          �
          
  �  �
                                 �             �                                                                                          �
            P  �
      �                        �  �             <                                                                                          �
            �  �
      x                        d  ,             �                                                                                          �
                �
      $                                         �                                                                                          �
                          ��                                               ��          �  ,  <                           
             
             
                                         
                                                                                                                <   L   \   l   |   �   �   �   �   �   �   �   �         <   L   \   l   |   �   �   �   �   �   �   �   �      ��                                                                                         ����                            �    �  2                 :�    +   �y    undefined                                                               �       �  �   l    �                        �����               ,s                    O   ����    e�          O   ����    R�          O   ����    ��      t       �   �              4   ����     /                                    3   ����       $     H  ���                       8      
                       � ߱        �  �      D       p
     <          assignFocusedWidget         �      �            LOGICAL,INPUT pcName CHARACTER  assignWidgetValue   �      �      $    $       LOGICAL,INPUT pcName CHARACTER,INPUT pcValue CHARACTER  assignWidgetValueList         \      �    6       LOGICAL,INPUT pcNameList CHARACTER,INPUT pcValueList CHARACTER,INPUT pcDelimiter CHARACTER  blankWidget t      �          L       LOGICAL,INPUT pcNameList CHARACTER  clearWidget �      @      l    X       LOGICAL,INPUT pcNameList CHARACTER  disableRadioButton  L      �      �    d       LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER    disableWidget   �            4    w       LOGICAL,INPUT pcNameList CHARACTER  enableRadioButton         X      �    �       LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER    enableWidget    l      �      �    �       LOGICAL,INPUT pcNameList CHARACTER  formattedWidgetValue    �             X  	  �       CHARACTER,INPUT pcName CHARACTER    formattedWidgetValueList    8      |      �  
  �       CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER    hideWidget  �      �      (   
 �       LOGICAL,INPUT pcNameList CHARACTER  highlightWidget       L      |    �       LOGICAL,INPUT pcNameList CHARACTER,INPUT pcHighlightType CHARACTER  resetWidgetValue    \      �      �    �       LOGICAL,INPUT pcNameList CHARACTER  toggleWidget    �            H    �       LOGICAL,INPUT pcNameList CHARACTER  viewWidget  (      l      �   
       LOGICAL,INPUT pcNameList CHARACTER  widgetHandle    x      �      �          HANDLE,INPUT pcName CHARACTER   widgetLongcharValue �            @    #      LONGCHAR,INPUT pcName CHARACTER widgetIsBlank          `      �    7      LOGICAL,INPUT pcNameList CHARACTER  widgetIsFocused p      �      �    E      LOGICAL,INPUT pcName CHARACTER  widgetIsModified    �      	      8	    U      LOGICAL,INPUT pcNameList CHARACTER  widgetIsTrue    	      \	      �	    f      LOGICAL,INPUT pcName CHARACTER  widgetValue l	      �	      �	    s      CHARACTER,INPUT pcName CHARACTER    widgetValueList �	      �	      ,
          CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER    P  $  �   �
  ���                       d                          � ߱            u   ����  �             p   �           |   �          �   �              � ߱            Z   �����
   ��
                     ��    �  l  �      �       4   �����                 �                      ��                  �  �                  �t                       �  |  |    �    $      �       4   �����       $  �  P  ���                       �   @         �               � ߱              �  �  �             4   ����       $  �  �  ���                       d  @         P              � ߱        assignPageProperty                              �  �      ��                      �              �~s                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             �               ��                  �           ��                            ����                            changePage                              �  �      ��                                     �s                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            confirmExit                             �  �      ��                                     ��u                    O   ����    e�          O   ����    R�          O   ����    ��            ��                             ��                            ����                            constructObject                               �      ��                    "  ,              <s                    O   ����    e�          O   ����    R�          O   ����    ��            ��   x             D               �� 
  �             l  
             ��   �             �               �� 
                 �  
         ��                            ����                            createObjects                               �  �      ��                  $  %  �              ��u                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            deletePage                              �  �      ��                  '  )  �              �Hs                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            destroyObject                               �  �      ��                  +  ,  �              �Tu                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hidePage                                �  �      ��                  .  0  �              lWu                    O   ����    e�          O   ����    R�          O   ����    ��            ��                             ��                            ����                            initializeObject                                  �      ��                  2  3  ,              �Mt                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeVisualContainer                               $        ��                  5  6  <              �pt                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initPages                               $        ��                  8  :  <              �qt                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  T           ��                            ����                            notifyPage                              L  4      ��                  <  >  d              l)v                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  |           ��                            ����                            passThrough                             t  \      ��                  @  C  �              �u                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             �               ��                  �           ��                            ����                            removePageNTarget                               �  �      ��                  E  H  �              $s                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  0             �  
             ��                  $           ��                            ����                            selectPage                                      ��                  J  L  4              (Tv                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  L           ��                            ����                            toolbar                             @  (      ��                  N  P  X              �Tv                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  p           ��                            ����                            viewObject                              h   P       ��                  R  S  �               x"u                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            viewPage                                h!  P!      ��                  U  W  �!              #u                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �!           ��                            ����                            disablePagesInFolder    
       "      8"    $      LOGICAL,INPUT pcPageInformation CHARACTER   enablePagesInFolder "      d"      �"    9      LOGICAL,INPUT pcPageInformation CHARACTER   getCallerProcedure  x"      �"      �"    M      HANDLE, getCallerWindow �"       #      0#    `      HANDLE, getContainerMode    #      8#      l#    p      CHARACTER,  getContainerTarget  L#      x#      �#    �      CHARACTER,  getContainerTargetEvents    �#      �#      �#    �      CHARACTER,  getCurrentPage  �#       $      0$    �      INTEGER,    getDisabledAddModeTabs  $      <$      t$     �      CHARACTER,  getDynamicSDOProcedure  T$      �$      �$  !  �      CHARACTER,  getFilterSource �$      �$      �$  "  �      HANDLE, getMultiInstanceActivated   �$      �$      8%  #  �      LOGICAL,    getMultiInstanceSupported   %      D%      �%  $        LOGICAL,    getNavigationSource `%      �%      �%  %  .      CHARACTER,  getNavigationSourceEvents   �%      �%      &  &  B      CHARACTER,  getNavigationTarget �%      &      H&  '  \      HANDLE, getOutMessageTarget (&      P&      �&  (  p      HANDLE, getPageNTarget  d&      �&      �&  )  �      CHARACTER,  getPageSource   �&      �&      �&  *  �      HANDLE, getPrimarySdoTarget �&       '      4'  +  �      HANDLE, getReEnableDataLinks    '      <'      t'  ,  �      CHARACTER,  getRunDOOptions T'      �'      �'  -  �      CHARACTER,  getRunMultiple  �'      �'      �'  .  �      LOGICAL,    getSavedContainerMode   �'      �'      0(  /  �      CHARACTER,  getSdoForeignFields (      <(      p(  0  �      CHARACTER,  getTopOnly  P(      |(      �(  1 
       LOGICAL,    getUpdateSource �(      �(      �(  2        CHARACTER,  getUpdateTarget �(      �(       )  3  .      CHARACTER,  getWaitForObject     )      ,)      `)  4  >      HANDLE, getWindowTitleViewer    @)      h)      �)  5  O      HANDLE, getStatusArea   �)      �)      �)  6  d      LOGICAL,    pageNTargets    �)      �)      *  7  r      CHARACTER,INPUT phTarget HANDLE,INPUT piPageNum INTEGER setCallerObject �)      L*      |*  8        LOGICAL,INPUT h HANDLE  setCallerProcedure  \*      �*      �*  9  �      LOGICAL,INPUT h HANDLE  setCallerWindow �*      �*      +  :  �      LOGICAL,INPUT h HANDLE  setContainerMode    �*      (+      \+  ;  �      LOGICAL,INPUT cContainerMode CHARACTER  setContainerTarget  <+      �+      �+  <  �      LOGICAL,INPUT pcObject CHARACTER    setCurrentPage  �+      �+      ,  =  �      LOGICAL,INPUT iPage INTEGER setDisabledAddModeTabs  �+      (,      `,  >  �      LOGICAL,INPUT cDisabledAddModeTabs CHARACTER    setDynamicSDOProcedure  @,      �,      �,  ?  �      LOGICAL,INPUT pcProc CHARACTER  setFilterSource �,      �,      -  @        LOGICAL,INPUT phObject HANDLE   setInMessageTarget  �,      8-      l-  A  #      LOGICAL,INPUT phObject HANDLE   setMultiInstanceActivated   L-      �-      �-  B  6      LOGICAL,INPUT lMultiInstanceActivated LOGICAL   setMultiInstanceSupported   �-      �-      4.  C  P      LOGICAL,INPUT lMultiInstanceSupported LOGICAL   setNavigationSource .      d.      �.  D  j      LOGICAL,INPUT pcSource CHARACTER    setNavigationSourceEvents   x.      �.      �.  E  ~      LOGICAL,INPUT pcEvents CHARACTER    setNavigationTarget �.      /      P/  F  �      LOGICAL,INPUT cTarget CHARACTER setOutMessageTarget 0/      p/      �/  G  �      LOGICAL,INPUT phObject HANDLE   setPageNTarget  �/      �/      �/  H  �      LOGICAL,INPUT pcObject CHARACTER    setPageSource   �/      0      H0  I  �      LOGICAL,INPUT phObject HANDLE   setPrimarySdoTarget (0      h0      �0  J  �      LOGICAL,INPUT hPrimarySdoTarget HANDLE  setReEnableDataLinks    |0      �0      �0  K  �      LOGICAL,INPUT cReEnableDataLinks CHARACTER  setRouterTarget �0      (1      X1  L        LOGICAL,INPUT phObject HANDLE   setRunDOOptions 81      x1      �1  M        LOGICAL,INPUT pcOptions CHARACTER   setRunMultiple  �1      �1      �1  N  &      LOGICAL,INPUT plMultiple LOGICAL    setSavedContainerMode   �1       2      X2  O  5      LOGICAL,INPUT cSavedContainerMode CHARACTER setSdoForeignFields 82      �2      �2  P  K      LOGICAL,INPUT cSdoForeignFields CHARACTER   setTopOnly  �2      �2      3  Q 
 _      LOGICAL,INPUT plTopOnly LOGICAL setUpdateSource �2      03      `3  R  j      LOGICAL,INPUT pcSource CHARACTER    setUpdateTarget @3      �3      �3  S  z      LOGICAL,INPUT pcTarget CHARACTER    setWaitForObject    �3      �3      4  T  �      LOGICAL,INPUT phObject HANDLE   setWindowTitleViewer    �3      ,4      d4  U  �      LOGICAL,INPUT phViewer HANDLE   getObjectType   D4      �4      �4  V  �      CHARACTER,  setStatusArea   �4      �4      �4  W  �      LOGICAL,INPUT plStatusArea LOGICAL  applyLayout                             �5  �5      ��                  �  �  �5              ��t                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disableObject                               �6  �6      ��                  �  �  �6              ��t                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            enableObject                                �7  �7      ��                  �  �  �7              ��t                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �8  �8      ��                  �  �  �8              ��u                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            processAction                               �9  �9      ��                  �  �  �9              p�u                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �9           ��                            ����                            getAllFieldHandles  �4      P:      �:  X  �      CHARACTER,  getAllFieldNames    d:      �:      �:  Y  �      CHARACTER,  getCol  �:      �:      �:  Z  �      DECIMAL,    getDefaultLayout    �:      ;      8;  [  �      CHARACTER,  getDisableOnInit    ;      D;      x;  \        LOGICAL,    getEnabledObjFlds   X;      �;      �;  ]        CHARACTER,  getEnabledObjHdls   �;      �;      �;  ^  +      CHARACTER,  getHeight   �;      <      0<  _ 	 =      DECIMAL,    getHideOnInit   <      <<      l<  `  G      LOGICAL,    getLayoutOptions    L<      x<      �<  a  U      CHARACTER,  getLayoutVariable   �<      �<      �<  b  f      CHARACTER,  getObjectEnabled    �<      �<      ,=  c  x      LOGICAL,    getObjectLayout =      8=      h=  d  �      CHARACTER,  getRow  H=      t=      �=  e  �      DECIMAL,    getWidth    |=      �=      �=  f  �      DECIMAL,    getResizeHorizontal �=      �=      >  g  �      LOGICAL,    getResizeVertical   �=       >      T>  h  �      LOGICAL,    setAllFieldHandles  4>      `>      �>  i  �      LOGICAL,INPUT pcValue CHARACTER setAllFieldNames    t>      �>      �>  j  �      LOGICAL,INPUT pcValue CHARACTER setDefaultLayout    �>      ?      <?  k  �      LOGICAL,INPUT pcDefault CHARACTER   setDisableOnInit    ?      `?      �?  l        LOGICAL,INPUT plDisable LOGICAL setHideOnInit   t?      �?      �?  m        LOGICAL,INPUT plHide LOGICAL    setLayoutOptions    �?      @      8@  n  #      LOGICAL,INPUT pcOptions CHARACTER   setObjectLayout @      \@      �@  o  4      LOGICAL,INPUT pcLayout CHARACTER    setResizeHorizontal l@      �@      �@  p  D      LOGICAL,INPUT plResizeHorizontal LOGICAL    setResizeVertical   �@      A      DA  q  X      LOGICAL,INPUT plResizeVertical LOGICAL  getObjectTranslated $A      lA      �A  r  j      LOGICAL,    getObjectSecured    �A      �A      �A  s  ~      LOGICAL,    createUiEvents  �A      �A      B  t  �      LOGICAL,    bindServer                              �B  �B      ��                  �  �  �B              l.t                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �C  �C      ��                  �  �  �C              �0t                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyServerObject                             �D  �D      ��                  �  �  �D              0v                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disconnectObject                                �E  �E      ��                  �  �  �E              �v                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              �F  �F      ��                  �  �  �F              4v                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            restartServerObject                             �G  �G      ��                  �  �  �G              0�t                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            runServerObject                             �H  �H      ��                  �  �  �H              ȕt                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 I  
         ��                            ����                            startServerObject                               J  �I      ��                  �  �  ,J              P�r                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            unbindServer                                K   K      ��                  �  �  0K              H�t                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  HK           ��                            ����                            getAppService   �A      �K      �K  u  �      CHARACTER,  getASBound  �K      �K      L  v 
 �      LOGICAL,    getAsDivision   �K      $L      TL  w  �      CHARACTER,  getASHandle 4L      `L      �L  x  �      HANDLE, getASHasStarted lL      �L      �L  y  �      LOGICAL,    getASInfo   �L      �L      �L  z 	 �      CHARACTER,  getASInitializeOnRun    �L      M      @M  {  �      LOGICAL,    getASUsePrompt   M      LM      |M  |   	      LOGICAL,    getServerFileName   \M      �M      �M  }  	      CHARACTER,  getServerOperatingMode  �M      �M       N  ~  !	      CHARACTER,  runServerProcedure  �M      N      @N    8	      HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE   setAppService    N      �N      �N  �  K	      LOGICAL,INPUT pcAppService CHARACTER    setASDivision   �N      �N      O  �  Y	      LOGICAL,INPUT pcDivision CHARACTER  setASHandle �N      0O      \O  �  g	      LOGICAL,INPUT phASHandle HANDLE setASInfo   <O      |O      �O  � 	 s	      LOGICAL,INPUT pcInfo CHARACTER  setASInitializeOnRun    �O      �O       P  �  }	      LOGICAL,INPUT plInitialize LOGICAL  setASUsePrompt  �O      $P      TP  �  �	      LOGICAL,INPUT plFlag LOGICAL    setServerFileName   4P      tP      �P  �  �	      LOGICAL,INPUT pcFileName CHARACTER  setServerOperatingMode  �P      �P      Q  �  �	      LOGICAL,INPUT pcServerOperatingMode CHARACTER   addLink                             �Q  �Q      ��                  �  �  �Q              �s                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  $R             �Q  
             ��   LR             R               �� 
                 @R  
         ��                            ����                            addMessage                              8S   S      ��                  �  �  PS              ��s                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �S             hS               ��   �S             �S               ��                  �S           ��                            ����                            adjustTabOrder                              �T  �T      ��                  �  �  �T              ��s                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  U             �T  
             �� 
  @U             U  
             ��                  4U           ��                            ����                            applyEntry                              ,V  V      ��                  �  �  DV              �tu                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  \V           ��                            ����                            changeCursor                                XW  @W      ��                  �  �  pW              �\s                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �W           ��                            ����                            createControls                              �X  lX      ��                  �  �  �X              @iu                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �Y  pY      ��                  �  �  �Y              �ku                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            displayLinks                                �Z  tZ      ��                  �  �  �Z              �t                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            editInstanceProperties                              �[  �[      ��                  �  �  �[              ��t                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            exitObject                              �\  �\      ��                  �  �  �\              �u                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hideObject                              �]  �]      ��                  �  �  �]              ��u                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �^  �^      ��                  �  �  �^              L�u                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            modifyListProperty                              �_  �_      ��                  �  �  �_              �t                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  `             �_  
             ��   4`              `               ��   \`             (`               ��                  P`           ��                            ����                            modifyUserLinks                             La  4a      ��                  �  �  da              �g�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �a             |a               ��   �a             �a               �� 
                 �a  
         ��                            ����                            removeAllLinks                              �b  �b      ��                  �  �  �b              X1�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            removeLink                              �c  �c      ��                  �  �  �c              �p�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  ,d             �c  
             ��   Td              d               �� 
                 Hd  
         ��                            ����                            repositionObject                                He  0e      ��                  �  �  `e              ,�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �e             xe               ��                  �e           ��                            ����                            returnFocus                             �f  �f      ��                  �  �  �f              ��                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �f  
         ��                            ����                            showMessageProcedure                                �g  �g      ��                  �  �  �g              ���                    O   ����    e�          O   ����    R�          O   ����    ��            ��   0h             �g               ��                  $h           ��                            ����                            toggleData                              i  i      ��                  �  �  4i              �                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  Li           ��                            ����                            viewObject                              Dj  ,j      ��                  �  �  \j              �ɕ                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            anyMessage  �P      �j      �j  � 
       LOGICAL,    assignLinkProperty  �j      �j       k  �  #      LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER   fetchMessages    k      xk      �k  �  6      CHARACTER,  getChildDataKey �k      �k      �k  �  D      CHARACTER,  getContainerHandle  �k      �k      $l  �  T      HANDLE, getContainerHidden  l      ,l      `l  �  g      LOGICAL,    getContainerSource  @l      ll      �l  �  z      HANDLE, getContainerSourceEvents    �l      �l      �l  �  �      CHARACTER,  getContainerType    �l      �l      $m  �  �      CHARACTER,  getDataLinksEnabled m      0m      dm  �  �      LOGICAL,    getDataSource   Dm      pm      �m  �  �      HANDLE, getDataSourceEvents �m      �m      �m  �  �      CHARACTER,  getDataSourceNames  �m      �m      n  �  �      CHARACTER,  getDataTarget   �m      (n      Xn  �         CHARACTER,  getDataTargetEvents 8n      dn      �n  �        CHARACTER,  getDBAware  xn      �n      �n  � 
 "      LOGICAL,    getDesignDataObject �n      �n      o  �  -      CHARACTER,  getDynamicObject    �n      o      Po  �  A      LOGICAL,    getInstanceProperties   0o      \o      �o  �  R      CHARACTER,  getLogicalObjectName    to      �o      �o  �  h      CHARACTER,  getLogicalVersion   �o      �o      p  �  }      CHARACTER,  getObjectHidden �o      $p      Tp  �  �      LOGICAL,    getObjectInitialized    4p      `p      �p  �  �      LOGICAL,    getObjectName   xp      �p      �p  �  �      CHARACTER,  getObjectPage   �p      �p      q  �  �      INTEGER,    getObjectParent �p      q      Lq  �  �      HANDLE, getObjectVersion    ,q      Tq      �q  �  �      CHARACTER,  getObjectVersionNumber  hq      �q      �q  �  �      CHARACTER,  getParentDataKey    �q      �q      r  �        CHARACTER,  getPassThroughLinks �q      r      Lr  �        CHARACTER,  getPhysicalObjectName   ,r      Xr      �r  �  -      CHARACTER,  getPhysicalVersion  pr      �r      �r  �  C      CHARACTER,  getPropertyDialog   �r      �r      s  �  V      CHARACTER,  getQueryObject  �r      s      Ls  �  h      LOGICAL,    getRunAttribute ,s      Xs      �s  �  w      CHARACTER,  getSupportedLinks   hs      �s      �s  �  �      CHARACTER,  getTranslatableProperties   �s      �s      t  �  �      CHARACTER,  getUIBMode  �s      t      Ht  � 
 �      CHARACTER,  getUserProperty (t      Tt      �t  �  �      CHARACTER,INPUT pcPropName CHARACTER    instancePropertyList    dt      �t      �t  �  �      CHARACTER,INPUT pcPropList CHARACTER    linkHandles �t      u      8u  �  �      CHARACTER,INPUT pcLink CHARACTER    linkProperty    u      \u      �u  �  �      CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER mappedEntry lu      �u      �u  �  �      CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER  messageNumber   �u      `v      �v  �        CHARACTER,INPUT piMessage INTEGER   propertyType    pv      �v      �v  �        CHARACTER,INPUT pcPropName CHARACTER    reviewMessages  �v      w      <w  �  #      CHARACTER,  setChildDataKey w      Hw      xw  �  2      LOGICAL,INPUT cChildDataKey CHARACTER   setContainerHidden  Xw      �w      �w  �  B      LOGICAL,INPUT plHidden LOGICAL  setContainerSource  �w      �w      (x  �  U      LOGICAL,INPUT phObject HANDLE   setContainerSourceEvents    x      Hx      �x  �  h      LOGICAL,INPUT pcEvents CHARACTER    setDataLinksEnabled dx      �x      �x  �  �      LOGICAL,INPUT lDataLinksEnabled LOGICAL setDataSource   �x      y      4y  �  �      LOGICAL,INPUT phObject HANDLE   setDataSourceEvents y      Ty      �y  �  �      LOGICAL,INPUT pcEventsList CHARACTER    setDataSourceNames  hy      �y      �y  �  �      LOGICAL,INPUT pcSourceNames CHARACTER   setDataTarget   �y      z      <z  �  �      LOGICAL,INPUT pcTarget CHARACTER    setDataTargetEvents z      `z      �z  �  �      LOGICAL,INPUT pcEvents CHARACTER    setDBAware  tz      �z      �z  � 
 �      LOGICAL,INPUT lAware LOGICAL    setDesignDataObject �z      {      8{  �  �      LOGICAL,INPUT pcDataObject CHARACTER    setDynamicObject    {      `{      �{  �        LOGICAL,INPUT lTemp LOGICAL setInstanceProperties   t{      �{      �{  �        LOGICAL,INPUT pcPropList CHARACTER  setLogicalObjectName    �{      |      D|  �  2      LOGICAL,INPUT c CHARACTER   setLogicalVersion   $|      `|      �|  �  G      LOGICAL,INPUT cVersion CHARACTER    setObjectName   t|      �|      �|  �  Y      LOGICAL,INPUT pcName CHARACTER  setObjectParent �|      }      8}  �  g      LOGICAL,INPUT phParent HANDLE   setObjectVersion    }      X}      �}  �  w      LOGICAL,INPUT cObjectVersion CHARACTER  setParentDataKey    l}      �}      �}  �  �      LOGICAL,INPUT cParentDataKey CHARACTER  setPassThroughLinks �}      ~      D~  �  �      LOGICAL,INPUT pcLinks CHARACTER setPhysicalObjectName   $~      d~      �~  �  �      LOGICAL,INPUT cTemp CHARACTER   setPhysicalVersion  |~      �~      �~  �  �      LOGICAL,INPUT cVersion CHARACTER    setRunAttribute �~            D  �  �      LOGICAL,INPUT cRunAttribute CHARACTER   setSupportedLinks   $      l      �  �  �      LOGICAL,INPUT pcLinkList CHARACTER  setTranslatableProperties   �      �       �  �  �      LOGICAL,INPUT pcPropList CHARACTER  setUIBMode  �      $�      P�  � 
       LOGICAL,INPUT pcMode CHARACTER  setUserProperty 0�      p�      ��  �        LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER  showmessage ��      ��      �  �  -      LOGICAL,INPUT pcMessage CHARACTER   Signature   �      0�      \�  � 	 9      CHARACTER,INPUT pcName CHARACTER    T�      ��  �      �      4   �����                (�                      ��                    =                  �~�                         ��          D�  ��      �      4   �����                Ђ                      ��                    <                  `�                         T�  Ѓ    )  �  h�      �      4   �����                x�                      ��                  5  7                  ��                       5  ��         6                                  �     
                    � ߱        ��  $  9  ��  ���                           $  ;  (�  ���                       �                         � ߱        `�    A  p�  �      �      4   �����                ��                      ��                  B  	                  ���                       B  ��  0�  o   E      ,                                 ��  $   F  \�  ���                       d  @         P              � ߱        ��  �   G  �      ��  �   H  �      ą  �   J  l      ؅  �   L  �      �  �   N  T       �  �   P  �      �  �   Q  D      (�  �   R  �      <�  �   U  �      P�  �   W  h      d�  �   X  �      x�  �   Z  `      ��  �   [  �      ��  �   \  	      ��  �   ]  �	      Ȇ  �   ^  
      ܆  �   d  D
      ��  �   f  �
      �  �   l  �
      �  �   n  h      ,�  �   p  �      @�  �   q  X      T�  �   w  �      h�  �   x  H      |�  �   y  �      ��  �   z  8      ��  �   }  �      ��  �   ~  �      ̇  �   �  \      ��  �   �  �      �  �   �        �  �   �  H      �  �   �  �      0�  �   �  �      D�  �   �  �      X�  �   �  x      l�  �   �  �      ��  �   �  �      ��  �   �  ,      ��  �   �  h      ��  �   �  �      Ј  �   �  �      �  �   �        ��  �   �  X          �   �  �                      $�          ��  x�      ��                  -	  [	  ��              <��                    O   ����    e�          O   ����    R�          O   ����    ��           
                �                     �                         � ߱        P�  $ A	  ��  ���                           O   Y	  ��  ��  �               ��          ��  ��    ��                                             ��                            ����                                �4      �      h�     6     Ċ                      V ��  �                      �    {	  |�  ��      �      4   �����                �                      ��                  |	  
                  ��                       |	  ��  �  �   	  <      0�  �   �	  �      D�  �   �	  ,      X�  �   �	  �      l�  �   �	  $      ��  �   �	  �      ��  �   �	        ��  �   �	  �      ��  �   �	        Ќ  �   �	  �      �  �   �	  �      ��  �   �	  x      �  �   �	  �          �   �	  p      ��    
  <�  ��      �      4   �����                ȍ                      ��                  
  �
                  L�u                       
  L�  ܍  �   
  @      ��  �   
  �      �  �   
  (      �  �   
  �      ,�  �   
        @�  �   
  �      T�  �   
         h�  �   
  |       |�  �   
  �       ��  �   
  d!      ��  �   
  �!      ��  �   
  T"      ̎  �   
  �"      ��  �   
  D#      �  �   
  �#      �  �   
  <$      �  �    
  �$      0�  �   !
  4%      D�  �   "
  �%      X�  �   #
  ,&      l�  �   $
  �&      ��  �   %
  $'      ��  �   &
  �'      ��  �   '
  (      ��  �   (
  �(      Џ  �   )
  )      �  �   *
  �)          �   +
  *      �    �
  �  ��      t*      4   ����t*                ��                      ��                  �
  Z                  @�u                       �
  $�  ��  �   �
  �*      Ȑ  �   �
  P+      ܐ  �   �
  �+      �  �   �
  @,      �  �   �
  �,      �  �   �
  (-      ,�  �   �
  �-      @�  �   �
  �-      T�  �   �
  L.      h�  �   �
  �.      |�  �   �
  �.      ��  �   �
  8/      ��  �   �
  �/      ��  �   �
  (0      ̑  �   �
  �0      ��  �   �
  1      ��  �   �
  �1      �  �   �
   2      �  �   �
  |2      0�  �   �
  �2      D�  �   �
  ,3      X�  �   �
  �3      l�  �   �
  4      ��  �   �
  P4      ��  �   �
  �4      ��  �   �
  5      ��  �   �
  D5      В  �   �
  �5      �  �   �
  �5      ��  �   �
  �5      �  �   �
  46       �  �   �
  p6      4�  �   �
  �6      H�  �   �
   7      \�  �   �
  \7      p�  �   �
  �7      ��  �   �
  �7      ��  �   �
  8      ��  �   �
  L8      ��  �   �
  �8      ԓ  �   �
  �8      �  �   �
  89      ��  �   �
  �9      �  �   �
   :      $�  �   �
  �:      8�  �   �
  ;      L�  �   �
  �;      `�  �   �
  <      t�  �   �
  �<      ��  �   �
   =      ��  �   �
  |=      ��  �   �
  �=      Ĕ  �   �
  4>      ؔ  �   �
  p>      �  �   �
  �>       �  �   �
  �>          �   �
  \?      l�  $  f  @�  ���                       �?     
                    � ߱        �    �  ��  ��      �?      4   �����?      /   �  ĕ     ԕ                          3   �����?            ��                      3   ����@  X�    �   �  ��  ��  $@      4   ����$@  	              ��                      ��             	     �  .                  �l�                       �  0�  ��  �   �  �@      �  $  �  �  ���                       �@     
                    � ߱        ,�  �   �  �@      ��  $   �  X�  ���                       �@  @         �@              � ߱        @�  $  �  ��  ���                       LA       	       	           � ߱        �A     
                <B                     �C  @        
 LC              � ߱        И  V   �  ܗ  ���                        �C       	       	       �C       
       
       D       	       	           � ߱        `�  $  �  l�  ���                       �D     
                DE                     �F  @        
 TF              � ߱        �  V   �  ��  ���                        �F     
                G                     lH  @        
 ,H              � ߱            V     ��  ���                        
              P�                      ��             
     0  �                  �f�                       0  �  �H     
                �H                     LJ  @        
 J          �J  @        
 pJ          K  @        
 �J          tK  @        
 4K              � ߱            V   E  ��  ���                        adm-clone-props �  |�              �     7     `                          \  k                     start-super-proc    ��  �  �           �     8                                  �                     �    �  t�  ��       O      4   ���� O      /   �  ��     ��                          3   ����O            ��                      3   ����0O  H�  $     �  ���                       PO                         � ߱        �      d�  ��  ��  lO      4   ����lO                T�                      ��                                      �o�                         t�  �O                     �O                     �O                         � ߱            $    �  ���                               ��  ؞      �O      4   �����O  �O                         � ߱            $    ��  ���                        �       �  0�  ��  �O      4   �����O      $    \�  ���                       P                         � ߱            �   <  (P      hP     
                �P                     4R  @        
 �Q              � ߱        ,�  V   P  ��  ���                        @�  �   �  @R      ؠ      \�  l�      �R      4   �����R      /     ��     ��                          3   �����R            Ƞ                      3   �����R  ��  $  
  �  ���                       �R                         � ߱        �R     
                tS                     �T  @        
 �T              � ߱        ��  V     0�  ���                        ��    �  ܡ  X�      �T      4   �����T                h�                      ��                  �  �                  ��u                       �  �      g   �  ��         ��D�                           H�          �   �      ��                  �      0�              @�u                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  t�     ��  �T                      3   �����T  ��     
   ��                      3   ����U         
   ԣ                      3   ����U    ��                              ��                           ����                                        ��              9      �                      g                               ��  g   �  ��          ��	L�                           ��          P�  8�      ��                  �  �  h�              ��u                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  ��     ��  0U                      3   ����U            ܥ                      3   ����8U    ��                              ��                           ����                                        ̤              :      �                      g                               ��  g   �  ��          ��	T�                           ��          X�  @�      ��                  �  �  p�              XV�                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  ��     ħ  pU                      3   ����TU            �                      3   ����xU    ��                              ��                           ����                                        Ԧ              ;      ��                      g                               �    �  ̨  H�      �U      4   �����U                X�                      ��                  �  �                  �V�                       �  ܨ  ĩ  /   �  ��     ��                          3   �����U            ��                      3   �����U  ��  /  �  �      �   V                      3   �����U  0�     
    �                      3   ����V  `�        P�                      3   ����V  ��        ��                      3   ����$V            ��                      3   ����HV  �    �  ܪ  �      lV      4   ����lV      /  �  �     (�  �V                      3   �����V  X�     
   H�                      3   �����V  ��        x�                      3   ����W  ��        ��                      3   ����W            ث                      3   ����<W        �  �  �      \W      4   ����\W      /  �  @�     P�  �W                      3   �����W  ��     
   p�                      3   �����W  ��        ��                      3   �����W  �        Ь                      3   �����W             �                      3   �����W  ��     �  X                                     (X     
                �X                     �Y  @        
 �Y              � ߱        8�  V   L  D�  ���                        Z     
                �Z                     �[  @        
 �[              � ߱        ��  V   s  ԭ  ���                        �[  @         �[          $\  @         \              � ߱        خ  $   �  d�  ���                       ��  g   �  �         �60�                            ��          ��  p�      ��                  �  �  ��              �pu                    O   ����    e�          O   ����    R�          O   ����    ��            �  8\  }        ��                              ��                           ����                                        �              <      Я                      g                               L�  g   �  ��         �"�                           ��          <�  $�      ��                 �  �  T�              Tsu                    O   ����    e�          O   ����    R�          O   ����    ��                                � ߱        ı  $   �  l�   �                       �  $  �  �  ���                       P\                         � ߱        t�  $  �  H�  ���                       \\                         � ߱              �  ��  �      h\      4   ����h\                �                      ��                  �  �                   �t                       �  ��  `�  	  �  P�                                        3   �����\  x�    �  �\           O  �  ������  ]    ��                              ��                           ����                                        ��              =      ��                      g                               ��    �  h�  �      ]      4   ����]                ��                      ��                  �                    �j�                       �  x�  8�  	  �  (�                                        3   ����0]  t�  /     d�                                 3   �����]  ��  �     �]      O     ��  ��  �]   �    	  ��  ȵ      �]      4   �����]      $   
  ��  ���                       0^  @         ^              � ߱        ȶ  /     L�                                 3   ����8^                �          �  ض      ��                                     �k�                x�       \�      O       ��          O       ��      D�  /     4�                                 3   ����T^      k     `�                    M�        �       /     ��                                 3   ����t^  adm-create-objects  ��  ��                      >      �                               �                     disable_UI  ȷ  $�                      ?      �                               �  
                   enable_UI   0�  ��                      @      L             �              �  	                    �  ���   ���  �               8   ����       8   ����       @�  L�      toggleData  ,INPUT plEnabled LOGICAL    0�  x�  ��      showMessageProcedure    ,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL  h�  Թ  �      returnFocus ,INPUT hTarget HANDLE   Ĺ  �  �      repositionObject    ,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL    ��  X�  d�      removeLink  ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE H�  ��  Ⱥ      removeAllLinks  ,   ��  ܺ  �      modifyUserLinks ,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE ̺  D�  X�      modifyListProperty  ,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER    4�  л  ܻ      hideObject  ,   ��  �  ��      exitObject  ,   �  �  (�      editInstanceProperties  ,    �  <�  L�      displayLinks    ,   ,�  `�  p�      createControls  ,   P�  ��  ��      changeCursor    ,INPUT pcCursor CHARACTER   t�  ��  ̼      applyEntry  ,INPUT pcField CHARACTER    ��  ��  �      adjustTabOrder  ,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER �  `�  l�      addMessage  ,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER P�  Ľ  ̽      addLink ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE ��   �  0�      unbindServer    ,INPUT pcMode CHARACTER �  X�  l�      startServerObject   ,   H�  ��  ��      runServerObject ,INPUT phAppService HANDLE  p�  ��  о      restartServerObject ,   ��  �  ��      initializeServerObject  ,   Ծ  �  $�      disconnectObject    ,    �  8�  L�      destroyServerObject ,   (�  `�  l�      bindServer  ,   P�  ��  ��      processAction   ,INPUT pcAction CHARACTER   p�  ��  ̿      enableObject    ,   ��  �  �      disableObject   ,   п  �  �      applyLayout ,   ��  $�  0�      viewPage    ,INPUT piPageNum INTEGER    �  \�  h�      viewObject  ,   L�  |�  ��      toolbar ,INPUT pcValue CHARACTER    l�  ��  ��      selectPage  ,INPUT piPageNum INTEGER    ��  ��  ��      removePageNTarget   ,INPUT phTarget HANDLE,INPUT piPage INTEGER ��  8�  D�      passThrough ,INPUT pcLinkName CHARACTER,INPUT pcArgument CHARACTER  (�  ��  ��      notifyPage  ,INPUT pcProc CHARACTER |�  ��  ��      initPages   ,INPUT pcPageList CHARACTER ��  ��  �      initializeVisualContainer   ,   ��  (�  <�      initializeObject    ,   �  P�  \�      hidePage    ,INPUT piPageNum INTEGER    @�  ��  ��      destroyObject   ,   x�  ��  ��      deletePage  ,INPUT piPageNum INTEGER    ��  ��  ��      createObjects   ,   ��  �  �      constructObject ,INPUT pcProcName CHARACTER,INPUT phParent HANDLE,INPUT pcPropList CHARACTER,OUTPUT phObject HANDLE ��  ��  ��      confirmExit ,INPUT-OUTPUT plCancel LOGICAL  |�  ��  ��      changePage  ,   ��  ��  ��      assignPageProperty  ,INPUT pcProp CHARACTER,INPUT pcValue CHARACTER      � 
"     
 u%     adecomm/as-utils.w 
"   
   �    }        �
"     
   � �  	   "      "      "          
�    
�        �     }         �     }        �     }             �     }        �%                  �     }         �     }        �     }             �     }        �%              � 
" 	   
 �%              � ��  �         �      \     H     $              
�    � C   �     
�             �G� C   �G     
�             �G                      
�            � E     
" 	   
 9
�H T   %              �     }        �GG %              � 
"   
   P �L 
�H T   %              �     }        �GG %              
"   
   �        D    7%               
"   
 Q�           x    1� U  
 Q� `   �%               o%   o           � e    Q
"   
 Q�           �    1� f   Q� `   �%               o%   o           � t   Q
"   
 Q�           `    1� {  
 Q� `   �%               o%   o           � �   Q
"   
 Q�           �    1� �   Q� `   �%               o%   o           � �  
 Q
"   
 Q�           H    1� �   Q� `   �%               o%   o           � �   Q
"   
 Q�           �    1� �   Q� �   �%               o%   o           %               
"   
 ��          8    1� �   �� �     
"   
 Q�           t    1� �   Q� `   �%               o%   o           �   e Q
"   
 Q�           �    1� u   Q� `   �%               o%   o           � �  ? Q
"   
 Q�           \    1� �   Q� �   �%               o%   o           %               
"   
 Q�           �    1� �   Q� �   �%               o%   o           %               
"   
 Q�           T    1� �   Q� �   �%               o%   o           %              
"   
 ��          �    1� �   �� �     
"   
 Q�           	    1�   
 Q� �   �%               o%   o           %               
"   
 Q�           �	    1�    Q� `   �%               o%   o           � e    Q
"   
 ��          �	    1�    �� �     
"   
 Q�           8
    1� %   Q� `   �%               o%   o           � ;  t Q
"   
 ��          �
    1� �  
 �� �     
"   
 Q�           �
    1� �   Q� `   �%               o%   o           � �  � Q
"   
 Q�           \    1� Y   Q� `   �%               o%   o           � e    Q
"   
 Q�           �    1� p  
 Q� {   �%               o%   o           %               
"   
 ��           L    1�    �� �   �%               o%   o           %               
"   
 ��           �    1� �   �� `   �%               o%   o           � e    �
"   
 ��           <    1� �   �� `   �%               o%   o           o%   o           
"   
 u�           �    1� �  
 u� `   �%               o%   o           � e    �
"   
 ��           ,    1� �   �� �  	 �%               o%   o           � �  / u
"   
 ��          �    1� �   �� �  	   
"   
 ��           �    1�    �� �  	 �o%   o           o%   o           � e    �
"   
 ��          P    1� #   �� �  	   
"   
 ��           �    1� 2   �� �  	 �o%   o           o%   o           � e    �
"   
 ��               1� B   �� �     
"   
 ��          <    1� P   �� �  	   
"   
 ��          x    1� ]   �� �  	   
"   
 ��          �    1� j   �� �  	   
"   
 ��           �    1� x   �� �   �o%   o           o%   o           %              
"   
 ��          l    1� �   �� �  	   
"   
 ��          �    1� �  
 �� �     
"   
 ��          �    1� �   �� �  	   
"   
 ��               1� �   �� �  	   
"   
 ��          \    1� �   �� �  	   
"   
 ��          �    1� �   �� �  	   
"   
 ��          �    1� �  	 �� �  	   
"   
 ��              1� �   �� �  	   
"   
 ��          L    1�    �� �  	   
"   
 ��           �    1� $   �� `   �%               o%   o           o%   o           
�H T   %              �     }        �GG %              
"   
   
"   
 �
"   
   
"   
 9(�  L ( l       �        P    �� 0   � P   �        \    �@    
� @  , 
�       h    �� 9     p�               �L
�    %              � 8      t    � $         � @          
�    � Z     
"   
 �� @  , 
�       �    �� {  
 �p�               �L"      P �L 
�H T   %              �     }        �GG %              
"   
 ��           0    1� ]  
 �� `   �%               o%   o           � e    �
"   
 ��           �    1� h  
 �� `   �%               o%   o           o%   o           
"   
 ��                1� s   �� �   �%               o%   o           o%   o           
"   
 ��           �    1� |   �� �   �%               o%   o           %               
"   
 ��               1� �   �� �   �%               o%   o           %               
"   
 u�           �    1� �   u� `   �%               o%   o           � e    �
"   
 ��               1� �   �� �   �%               o%   o           %              
"   
 ��           �    1� �   �� �   �%               o%   o           o%   o           
"   
 u�                1� �   u� `   �%               o%   o           o%   o           
"   
 ��           |    1� �  	 �� `   �%               o%   o           � e    �
"   
 ��           �    1� �   �� `   �%               o%   o           o%   o           
"   
 ��           l    1� �   �� `   �%               o%   o           o%   o           
"   
 ��           �    1� �   �� �   �%               o%   o           %               
"   
 ��           d    1�    �� �   �%               o%   o           o%   o           P �L 
�H T   %              �     }        �GG %              
"   
 ��           4    1�    �� �  	 �%               o%   o           � e    �
"   
 u�           �    1� !   u� �  	 �%               o%   o           � e    �
"   
 ��               1� /   �� �   �%               o%   o           %               
"   
 u�           �    1� =   u� �  	 �%               o%   o           � e    �
"   
 ��               1� L   �� �  	 �%               o%   o           � e    u
"   
 ��           �    1� Z   �� �   �%               o%   o           %               
"   
 ��           �    1� h   �� �  	 �%               o%   o           � e    �
"   
 ��           p     1� w   �� �  	 �%               o%   o           � e    �
"   
 ��           �     1� �   �� �  	 �%               o%   o           � e    �
"   
 ��           X!    1� �   �� �  	 �%               o%   o           o%   o           
"   
 ��           �!    1� �   �� �  	 �%               o%   o           � e    u
"   
 u�           H"    1� �   u� �  	 �%               o%   o           � e    �
"   
 ��           �"    1� �  	 �� �   �%               o%   o           %               
"   
 ��           8#    1� �   �� �   �%               o%   o           %               
"   
 ��           �#    1� �   �� �   �%               o%   o           o%   o           
"   
 ��           0$    1� �   �� �   �%               o%   o           o%   o           
"   
 ��           �$    1� �   �� �   �%               o%   o           %               
"   
 u�           (%    1�    u� �   �%               o%   o           %               
"   
 ��           �%    1�    �� �   �%               o%   o           %               
"   
 u�            &    1� '   u� 3   �%               o%   o           %       
       
"   
 u�           �&    1� ;   u� 3   �%               o%   o           o%   o           
"   
 ��           '    1� G   �� 3   �%               o%   o           %              
"   
 ��           �'    1� S   �� 3   �%               o%   o           o%   o           
"   
 ��           (    1� _   �� 3   �%               o%   o           %              
"   
 ��           �(    1� l   �� 3   �%               o%   o           o%   o           
"   
 u�           )    1� y   u� 3   �%               o%   o           %              
"   
 u�           �)    1� �   u� 3   �%               o%   o           o%   o           
"   
 u�            *    1� �   u� �  	 �%               o%   o           � e    �P �L 
�H T   %              �     }        �GG %              
"   
 ��           �*    1� �   �� {   �%               o%   o           %               
"   
 ��           D+    1� �   �� {   �%               o%   o           o%   o           
"   
 ��           �+    1� �   �� `   �%               o%   o           � e    �
"   
 ��           4,    1� �   �� `   �%               o%   o           � �  - �
"   
 ��           �,    1�    �� `   �%               o%   o           � e    �
"   
 u�           -    1�    u� `   �%               o%   o           � ;   �
"   
 ��          �-    1� Y   �� �     
"   
 ��           �-    1� j   �� `   �%               o%   o           � e    �
"   
 ��          @.    1� v  
 �� �     
"   
 ��          |.    1� �   �� �     
"   
 ��           �.    1� �   �� �  	 �%               o%   o           � e    �
"   
 ��           ,/    1� �   �� `   �%               o%   o           � e    �
"   
 ��           �/    1� �   �� �   �%               o%   o           o%   o           
"   
 u�           0    1� �   u� `   �%               o%   o           � �  ! �
"   
 ��           �0    1� �   �� `   �%               o%   o           � e    u
"   
 u�           1    1� �   u� `   �%               o%   o           � 
   �
"   
 u�           x1    1�   	 u� {   �%               o%   o           o%   o           
"   
 ��           �1    1� #   �� �   �%               o%   o           %               
"   
 ��          p2    1� /   �� �     
"   
 ��           �2    1� =   �� `   �%               o%   o           � Q   �
"   
 ��            3    1� `   �� �  	 �%               o%   o           � e    �
"   
 u�           �3    1� m   u� �  	 �%               o%   o           � e    �
"   
 ��          4    1� }   �� �     
"   
 ��          D4    1� �   �� �  	   
"   
 u�           �4    1� �   u� �   �o%   o           o%   o           %               
"   
 ��          �4    1� �   �� �     
"   
 ��          85    1� �   �� �  	   
"   
 ��          t5    1� �   �� �  	   
"   
 ��          �5    1� �   �� �  	   
"   
 ��          �5    1�    �� �  	   
"   
 ��          (6    1�    �� �  	   
"   
 ��          d6    1� $   �� �     
"   
 u�           �6    1� 5   u� `   �%               o%   o           � L  4 �
"   
 ��          7    1� �   �� �     
"   
 ��          P7    1� �   �� �     
"   
 ��          �7    1� �   �� �     
"   
 ��          �7    1� �   �� �  	   
"   
 ��          8    1� �   �� �  	   
"   
 ��          @8    1� �   �� �  	   
"   
 ��          |8    1� �   �� �     
"   
 ��           �8    1� �   �� �  	 �%               o%   o           � e    �
"   
 ��           ,9    1� �   �� �  	 �%               o%   o           � e    �
"   
 ��           �9    1� 
   �� �  	 �%               o%   o           � e    �
"   
 u�           :    1�    u� �  	 �%               o%   o           � e    �
"   
 ��           �:    1� 4   �� �   �%               o%   o           %               
"   
 ��           ;    1� B   �� �   �%               o%   o           o%   o           
"   
 ��           �;    1� T   �� �   �%               o%   o           %               
"   
 ��           �;    1� d   �� �   �%               o%   o           %               
"   
 ��           x<    1� p   �� �   �%               o%   o           o%   o           
"   
 ��           �<    1� �   �� �   �%               o%   o           %               
"   
 ��          p=    1� �   �� �  	   
"   
 ��           �=    1� �   �� �   �%               o%   o           %              
"   
 ��          (>    1� �   �� �  	   
"   
 ��          d>    1� �   �� �  	   
"   
 ��          �>    1� �  
 �� �  	   
"   
 ��           �>    1� �   �� �  	 �%               o%   o           � 4   �
"   
 ��           P?    1� �   �� �  	 �%               o%   o           � e    �
�             �G "    �%     start-super-proc ��%     adm2/smart.p �8P �L 
�H T   %              �     }        �GG %              
"   
   �       x@    6� 0     
"   
   
�        �@    8
"   
   �        �@    ��     }        �G 4              
"   
 ߱G %              G %              %p e `   LogicalObjectName,PhysicalObjectName,DynamicObject,RunAttribute,HideOnInit,DisableOnInit,ObjectLayout 8
�H T   %              �     }        �GG %              
"   
 9
"   
 �
"   
 9
"   
   (�  L ( l       �        B    �� 0   � P   �        B    �@    
� @  , 
�       $B    �� 9   9p�               �L
�    %              � 8      0B    � $         � @          
�    � Z   9
"   
 �p� @  , 
�       @C    �� �   �p�               �L"  	  , �   � -   �� /   ��     }        �A      |    "  	    � -   �%              (<   \ (    |    �     }        �A� 1   �A"  
  �    "  	  8"  
  �  < "  	  8"  
  �(    |    �     }        �A� 1   �A"  
  �
�H T   %              �     }        �GG %              
"   
 9
"   
 �
"   
 9
"   
   (�  L ( l       �        E    �� 0   � P   �         E    �@    
� @  , 
�       ,E    �� 9   9p�               �L
�    %              � 8      8E    � $         � @          
�    � Z   9
"   
 �p� @  , 
�       HF    �� U  
 �p�               �L"  	  , 
�H T   %              �     }        �GG %              
"   
 9
"   
 �
"   
 9
"   
 u(�  L ( l       �        �F    �� 0   � P   �        �F    �@    
� @  , 
�       G    �� 9   9p�               �L
�    %              � 8      G    � $         � @   9     
�    � Z   �
"   
 �p� @  , 
�        H    �� �   �p�               �L
�             �G
�H T   %              �     }        �GG %              
"   
   
"   
 �
"   
   
"   
   (�  L ( l       �        �H    �� 0   � P   �        �H    �@    
� @  , 
�       �H    �� 9     p�               �L
�    %              � 8      �H    � $         � @          
�    � Z     
"   
 �p� @  , 
�        J    �� {  
 �p�               �L%     SmartDialog 
"   
   p� @  , 
�       dJ    �� �     p�               �L% 
    DIALOG-BOX  
"   
  p� @  , 
�       �J    �� 2    p�               �L%               
"   
  p� @  , 
�       (K    ��     p�               �L(        � e      � e      � e      �     }        �A
�H T   %              �     }        �GG %              
"   
 � (   � 
"   
 8    �        L    �� 0   �
"   
   � 8      TL    � $         � @          
�    � Z   8
"   
   �        �L    �
"   
   �       �L    /
"   
   
"   
   �       �L    6� 0     
"   
   
�        $M    8
"   
   �        DM    �
"   
   �       dM    �
"   
   p�    � Z   �
�    �     }        �G 4              
"   
 ߱G %              G %              
�     }        �
"   
    (   � 
"   
 8    �        (N    �A"    �A
"   
   
�        tN    �@ � 
"   
 �"      �       }        �
"   
 �%              %                "    �%     start-super-proc ��%     adm2/appserver.p u��    � �     
�    �     }        �%               %      Server  - �     }        �    "    u� e    �%                   "    u� e    �%      NONE    p�,  8         $     "    u        � �   8
�    
�H T   %              �     }        �GG %              
"   
 9
"   
 �
"   
 9
"   
   (�  L ( l       �        �P    �� 0   � P   �        �P    �@    
� @  , 
�       �P    �� 9   9p�               �L
�    %              � 8      �P    � $         � @          
�    � Z   9
"   
 �p� @  , 
�       �Q    �� �   �p�               �L"    , p�,  8         $     "    u        �    8
�     "    �%     start-super-proc ��%     adm2/visual.p 8�   � C     � '     � )  (   
�H T   %              �     }        �GG %              
"   
 9
"   
 �
"   
 9
"   
   (�  L ( l       �        DS    �� 0   � P   �        PS    �@    
� @  , 
�       \S    �� 9   9p�               �L
�    %              � 8      hS    � $         � @          
�    � Z   9
"   
 �p� @  , 
�       xT    �� h   �p�               �L"    , � 
"    
 �%     contextHelp 
"    
   
�    
�    %     processAction   
�    %     CTRL-PAGE-UP �8%     processAction   
�    %     CTRL-PAGE-DOWN  "    �%     start-super-proc �%     adm2/containr.p %     modifyListProperty 
�    
�    %      Add     %      ContainerSourceEvents �%      initializeDataObjects �0 0   A    �    � �   �
�    � �   �A    �    � �     
�    � �   �%     modifyListProperty 
�    
�    %      Add     %      ContainerSourceEvents �%     buildDataRequest ent0 A    �    � �   �
�    � �   �%     modifyListProperty 
�    
�    %      Add     %     SupportedLinks %      ContainerToolbar-Target %               
�H T   %              �     }        �GG %              
"   
 9
"   
 �
"   
 9
"   
 �(�  L ( l       �        tX    �� 0   � P   �        �X    �@    
� @  , 
�       �X    �� 9   9p�               �L
�    %              � 8      �X    � $         � @   9     
�    � Z   �
"   
 �p� @  , 
�       �Y    �� }   �p�               �L
�             �G
�H T   %              �     }        �GG %              
"   
 9
"   
 �
"   
 9
"   
 9(�  L ( l       �        TZ    �� 0   � P   �        `Z    �@    
� @  , 
�       lZ    �� 9   9p�               �L
�    %              � 8      xZ    � $         � @   9     
�    � Z   9
"   
 �p� @  , 
�       �[    �� 4   �p�               �L%              �             I%               �             �%              % 	    END-ERROR �"      "      < <        S    "    ��    8%                    %                  "    ��     ��      %      ENTRY   %               �     }        � `     @     ,         � ?  (   G %       
       � h  &   G %       
       � �  & �% 
    disable_UI 
�    %                0   � 
�        
�             � 
%   
           
�             � 
�    %     createObjects   %     initializeObject �%     destroyObject   "    �� �   �� �   �� �   �&    &    @            "       &        "       &        "       &    "       "                       �           �   l       ��                 =  a  �               �h�                    O   ����    e�          O   ����    R�          O   ����    ��        $  L  �   ���                       �K     
                    � ߱              M  (  �      L      4   ����L                �                      ��                  N  `                  �ђ                       N  8  �  �  O  `L            Q  �  `      �L      4   �����L                p                      ��                  R  _                  DҒ                       R  �  �  o   S      ,                                 �  �   T  �L      �  �   U  M      $  $  V  �  ���                       0M     
                    � ߱        8  �   W  PM      L  �   X  pM      `  �   [  �M          $   ^  �  ���                       �M  @         �M              � ߱                     T          ,  @   T �            
             
             
             
                 $   4   D          $   4   D   ����        ��                            ����                                            �           �   l       ��                 �  �  �               t��                    O   ����    e�          O   ����    R�          O   ����    ��      {                      �          �  $  �    ���                       N     
                    � ߱                  �  �                      ��                   �  �                  ,��                     �  4      4   ����4N      $  �  �  ���                       �N     
                    � ߱        �    �  4  D      �N      4   �����N      /  �  p                               3   �����N  �  �   �  �N          O   �  ��  ��  �N                               , �                          
                               �      ��                            ����                                                        �   l       ��                  "  )  �               @l�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                                            �           �   l       ��                  /  :  �               m�                    O   ����    e�          O   ����    R�          O   ����    ��             9  �� �                   ��                              ��                           ����                                            �           �   l       ��                  @  P  �               �m�                    O   ����    e�          O   ����    R�          O   ����    ��      �^  �              � ߱        P  Z   J  �    �                            �               �              �              �              � ߱        |  h   L      �                        �  
   N  �� �                    s   O  �        0      H              �  H       ��                            7   ����           ��                �^   �            �                  6   O         �   ��               �^   �            �                                                                  �           �^           �^                      �   �          4_  @_                 �^   �^   �^      ��                              ��                           ����                            �        2                 :�        ��          �  $   �P                              
 �                                                                 �  �    �       K�                                    
 �                                                                �  �    �  (       �                                    
 �                                                                �      �  
                                           �                                                                                                                                       �    d d     �   ��%  �%  � �         �                                                                                               
   d     D                                                                 H  �  ��                                  �          �           \  L� �s                                 �                                  A      \  LK�s                                 �                                  B      P    �Xd                                                           %  G   
 X   �l d                                                        �            D                                                                                            TXS appSrvUtils ASSIGNFOCUSEDWIDGET ASSIGNWIDGETVALUE ASSIGNWIDGETVALUELIST BLANKWIDGET CLEARWIDGET DISABLERADIOBUTTON DISABLEWIDGET ENABLERADIOBUTTON ENABLEWIDGET FORMATTEDWIDGETVALUE FORMATTEDWIDGETVALUELIST HIDEWIDGET HIGHLIGHTWIDGET RESETWIDGETVALUE TOGGLEWIDGET VIEWWIDGET WIDGETHANDLE WIDGETLONGCHARVALUE WIDGETISBLANK WIDGETISFOCUSED WIDGETISMODIFIED WIDGETISTRUE WIDGETVALUE WIDGETVALUELIST pFlgEstDet pGlosa ADM-ERROR Btn_Cancel Btn_OK FILL-IN-Glosa almtabla Tablas de Almacen BROWSE-2 x(8) x(40) x(10) gDialog SELECCIONE UN MOTIVO X(256) DISABLEPAGESINFOLDER ENABLEPAGESINFOLDER GETCALLERPROCEDURE GETCALLERWINDOW GETCONTAINERMODE GETCONTAINERTARGET GETCONTAINERTARGETEVENTS GETCURRENTPAGE GETDISABLEDADDMODETABS GETDYNAMICSDOPROCEDURE GETFILTERSOURCE GETMULTIINSTANCEACTIVATED GETMULTIINSTANCESUPPORTED GETNAVIGATIONSOURCE GETNAVIGATIONSOURCEEVENTS GETNAVIGATIONTARGET GETOUTMESSAGETARGET GETPAGENTARGET GETPAGESOURCE GETPRIMARYSDOTARGET GETREENABLEDATALINKS GETRUNDOOPTIONS GETRUNMULTIPLE GETSAVEDCONTAINERMODE GETSDOFOREIGNFIELDS GETTOPONLY GETUPDATESOURCE GETUPDATETARGET GETWAITFOROBJECT GETWINDOWTITLEVIEWER GETSTATUSAREA PAGENTARGETS SETCALLEROBJECT SETCALLERPROCEDURE SETCALLERWINDOW SETCONTAINERMODE SETCONTAINERTARGET SETCURRENTPAGE SETDISABLEDADDMODETABS SETDYNAMICSDOPROCEDURE SETFILTERSOURCE SETINMESSAGETARGET SETMULTIINSTANCEACTIVATED SETMULTIINSTANCESUPPORTED SETNAVIGATIONSOURCE SETNAVIGATIONSOURCEEVENTS SETNAVIGATIONTARGET SETOUTMESSAGETARGET SETPAGENTARGET SETPAGESOURCE SETPRIMARYSDOTARGET SETREENABLEDATALINKS SETROUTERTARGET SETRUNDOOPTIONS SETRUNMULTIPLE SETSAVEDCONTAINERMODE SETSDOFOREIGNFIELDS SETTOPONLY SETUPDATESOURCE SETUPDATETARGET SETWAITFOROBJECT SETWINDOWTITLEVIEWER GETOBJECTTYPE SETSTATUSAREA GETALLFIELDHANDLES GETALLFIELDNAMES GETCOL GETDEFAULTLAYOUT GETDISABLEONINIT GETENABLEDOBJFLDS GETENABLEDOBJHDLS GETHEIGHT GETHIDEONINIT GETLAYOUTOPTIONS GETLAYOUTVARIABLE GETOBJECTENABLED GETOBJECTLAYOUT GETROW GETWIDTH GETRESIZEHORIZONTAL GETRESIZEVERTICAL SETALLFIELDHANDLES SETALLFIELDNAMES SETDEFAULTLAYOUT SETDISABLEONINIT SETHIDEONINIT SETLAYOUTOPTIONS SETOBJECTLAYOUT SETRESIZEHORIZONTAL SETRESIZEVERTICAL GETOBJECTTRANSLATED GETOBJECTSECURED CREATEUIEVENTS GETAPPSERVICE GETASBOUND GETASDIVISION GETASHANDLE GETASHASSTARTED GETASINFO GETASINITIALIZEONRUN GETASUSEPROMPT GETSERVERFILENAME GETSERVEROPERATINGMODE RUNSERVERPROCEDURE SETAPPSERVICE SETASDIVISION SETASHANDLE SETASINFO SETASINITIALIZEONRUN SETASUSEPROMPT SETSERVERFILENAME SETSERVEROPERATINGMODE gshAstraAppserver gshSessionManager gshRIManager gshSecurityManager gshProfileManager gshRepositoryManager gshTranslationManager gshWebManager gscSessionId gsdSessionObj gshFinManager gshGenManager gshAgnManager gsdTempUniqueID gsdUserObj gsdRenderTypeObj gsdSessionScopeObj ghProp ghADMProps ghADMPropsBuf glADMLoadFromRepos glADMOk ANYMESSAGE ASSIGNLINKPROPERTY FETCHMESSAGES GETCHILDDATAKEY GETCONTAINERHANDLE GETCONTAINERHIDDEN GETCONTAINERSOURCE GETCONTAINERSOURCEEVENTS GETCONTAINERTYPE GETDATALINKSENABLED GETDATASOURCE GETDATASOURCEEVENTS GETDATASOURCENAMES GETDATATARGET GETDATATARGETEVENTS GETDBAWARE GETDESIGNDATAOBJECT GETDYNAMICOBJECT GETINSTANCEPROPERTIES GETLOGICALOBJECTNAME GETLOGICALVERSION GETOBJECTHIDDEN GETOBJECTINITIALIZED GETOBJECTNAME GETOBJECTPAGE GETOBJECTPARENT GETOBJECTVERSION GETOBJECTVERSIONNUMBER GETPARENTDATAKEY GETPASSTHROUGHLINKS GETPHYSICALOBJECTNAME GETPHYSICALVERSION GETPROPERTYDIALOG GETQUERYOBJECT GETRUNATTRIBUTE GETSUPPORTEDLINKS GETTRANSLATABLEPROPERTIES GETUIBMODE GETUSERPROPERTY INSTANCEPROPERTYLIST LINKHANDLES LINKPROPERTY MAPPEDENTRY MESSAGENUMBER PROPERTYTYPE REVIEWMESSAGES SETCHILDDATAKEY SETCONTAINERHIDDEN SETCONTAINERSOURCE SETCONTAINERSOURCEEVENTS SETDATALINKSENABLED SETDATASOURCE SETDATASOURCEEVENTS SETDATASOURCENAMES SETDATATARGET SETDATATARGETEVENTS SETDBAWARE SETDESIGNDATAOBJECT SETDYNAMICOBJECT SETINSTANCEPROPERTIES SETLOGICALOBJECTNAME SETLOGICALVERSION SETOBJECTNAME SETOBJECTPARENT SETOBJECTVERSION SETPARENTDATAKEY SETPASSTHROUGHLINKS SETPHYSICALOBJECTNAME SETPHYSICALVERSION SETRUNATTRIBUTE SETSUPPORTEDLINKS SETTRANSLATABLEPROPERTIES SETUIBMODE SETUSERPROPERTY SHOWMESSAGE SIGNATURE , prepareInstance ObjectName CHAR  ObjectVersion ADM2.2 ObjectType SmartDialog ContainerType DIALOG-BOX PropertyDialog adm2/support/visuald.w QueryObject LOGICAL ContainerHandle HANDLE InstanceProperties LogicalObjectName,PhysicalObjectName,DynamicObject,RunAttribute,HideOnInit,DisableOnInit,ObjectLayout SupportedLinks Data-Target,Data-Source,Page-Target,Update-Source,Update-Target ContainerHidden ObjectInitialized ObjectHidden ObjectsCreated HideOnInit UIBMode ContainerSource ContainerSourceEvents initializeObject,hideObject,viewObject,destroyObject,enableObject,confirmExit,confirmCancel,confirmOk,isUpdateActive DataSource DataSourceEvents dataAvailable,queryPosition,updateState,deleteComplete,fetchDataSet,confirmContinue,confirmCommit,confirmUndo,assignMaxGuess,isUpdatePending TranslatableProperties ObjectPage INT DBAware DesignDataObject DataSourceNames DataTarget DataTargetEvents CHARACTER updateState,rowObjectState,fetchBatch,LinkState LogicalObjectName PhysicalObjectName LogicalVersion PhysicalVersion DynamicObject RunAttribute ChildDataKey ParentDataKey DataLinksEnabled InactiveLinks InstanceId DECIMAL SuperProcedure SuperProcedureMode SuperProcedureHandle LayoutPosition ClassName RenderingProcedure ThinRenderingProcedure Label cType ADMProps Target WHERE Target = WIDGET-H(" ") AppService ASDivision ASHandle ASHasConnected ASHasStarted ASInfo ASInitializeOnRun ASUsePrompt BindSignature BindScope ServerOperatingMode ServerFileName ServerFirstCall NeedContext ObjectLayout LayoutOptions ObjectEnabled LayoutVariable DefaultLayout DisableOnInit EnabledObjFlds EnabledObjHdls FieldSecurity SecuredTokens AllFieldHandles AllFieldNames MinHeight MinWidth ResizeHorizontal ResizeVertical ObjectSecured ObjectTranslated PopupButtonsInFields ColorInfoBG INTEGER ColorInfoFG ColorWarnBG ColorWarnFG ColorErrorBG ColorErrorFG BGColor FGColor FieldPopupMapping CurrentPage PendingPage ContainerTarget ContainerTargetEvents exitObject,okObject,cancelObject,updateActive ContainerToolbarSource ContainerToolbarSourceEvents toolbar,okObject,cancelObject OutMessageTarget PageNTarget PageSource FilterSource UpdateSource UpdateTarget CommitSource CommitSourceEvents commitTransaction,undoTransaction CommitTarget CommitTargetEvents rowObjectState StartPage RunMultiple WaitForObject DynamicSDOProcedure adm2/dyndata.w RunDOOptions InitialPageList WindowFrameHandle Page0LayoutManager MultiInstanceSupported MultiInstanceActivated ContainerMode SavedContainerMode SdoForeignFields NavigationSource NavigationTarget PrimarySdoTarget NavigationSourceEvents fetchFirst,fetchNext,fetchPrev,fetchLast,startFilter CallerWindow CallerProcedure CallerObject DisabledAddModeTabs ReEnableDataLinks WindowTitleViewer UpdateActive InstanceNames ClientNames ContainedDataObjects ContainedAppServices DataContainer HasDbAwareObjects HasDynamicProxy HideOnClose HideChildContainersOnClose HasObjectMenu RequiredPages RemoveMenuOnHide ProcessList PageLayoutInfo PageTokens DataContainerName WidgetIDFileName ghContainer adm2/smart.p cObjectName iStart / \ . hReposBuffer hPropTable hBuffer hTable deleteProperties ADM-CLONE-PROPS pcProcName hProc START-SUPER-PROC cAppService cASDivision cServerOperatingMode adm2/appserver.p getAppService Server NONE setASDivision setAppService cFields adm2/visual.p   BROWSE-2 Btn_OK Btn_Cancel FILL-IN-Glosa CTRL-PAGE-UP CTRL-PAGE-DOWN adm2/containr.p Add initializeDataObjects getSupportedLinks data-target data-source buildDataRequest containertoolbar-target ContainerToolbar-Target END-ERROR C00,L00  La glosa es obligatoria ENTRY iStartPage A SmartDialog is not intended to be run  Persistent or to be placed in another  SmartObject at AppBuilder design time. ADM-CREATE-OBJECTS DISABLE_UI HRD D I ENABLE_UI Codigo Codigo Nombre Nombre Area Responsable CodCta2 OK Cancel Glosa tabl01 �
  �      �"      % �8   ��      0         pcProp      ��      P         pcProp      ��      p         plCancel    �   ��      �         pcProcName  �   ��      �        
 pcProcName  �   ��      �         pcProcName      ��              
 pcProcName      ��      $        piPageNum       ��      H        piPageNum       ��      l        pcPageList      ��      �        pcProc  �  ��      �        pcLinkName      ��      �        pcLinkName    ��      �       
 phTarget        ��              phTarget        ��      @        piPageNum       ��      d        pcValue     ��      �        piPageNum       ��      �        pcAction        ��      �       
 phAppService        ��      �        pcMode     ��             
 phSource    D  ��      8        phSource        ��      \       
 phSource    �  ��      �        pcText  �  ��      �        pcText      ��      �        pcText  �  ��      �       
 phObject      ��             
 phObject        ��      (        phObject        ��      L        pcField     ��      l        pcCursor    �  ��      �       
 phCaller    �  ��      �        phCaller    �  ��      �        phCaller        ��      �        phCaller    (  ��               pcMod   H  ��      @        pcMod       ��      `       
 pcMod   �  ��      �       
 phSource    �  ��      �        phSource        ��      �       
 phSource    �  ��      �        pdRow       ��              pdRow       ��      ,       
 hTarget X  ��      L        pcMessage       ��      p        pcMessage       ��      �        plEnabled             �     cType       �     6   �          �                  getObjectType   A	  Y	  [	  ,          
   hReposBuffer    L        @  
   hPropTable  h        `  
   hBuffer           |  
   hTable  �  �     7             �                  adm-clone-props L  M  N  O  Q  R  S  T  U  V  W  X  [  ^  _  `  a              
   hProc             <        pcProcName  �  �  	   8     $      x                  start-super-proc    �  �  �  �  �  �  �  �  �  H  �     9                                   �  �  	     :                                   �  �  �  L	     ;                                   �  �  	  �	     <                                   �  �  T	  �	  
   =                                   �  �  �  �  �  �  �  �  �  �  �	  (
     >               
                  adm-create-objects  )  �	  h
     ?               \
                  disable_UI  9  :  ,
  �
     @               �
                  enable_UI   J  L  N  O  P  p
  �  �      �
  d  �                                  
   appSrvUtils 4       $     FILL-IN-Glosa   \        H  
   gshAstraAppserver   �        p  
   gshSessionManager   �        �  
   gshRIManager    �        �  
   gshSecurityManager  �        �  
   gshProfileManager   $  	 	       
   gshRepositoryManager    P  
 
     8  
   gshTranslationManager   t        d  
   gshWebManager   �        �     gscSessionId    �        �     gsdSessionObj   �        �  
   gshFinManager           �  
   gshGenManager   (          
   gshAgnManager   L        <     gsdTempUniqueID l        `     gsdUserObj  �        �     gsdRenderTypeObj    �        �     gsdSessionScopeObj  �       �  
   ghProp  �       �  
   ghADMProps           
   ghADMPropsBuf   D       0     glADMLoadFromRepos  `       X     glADMOk �       t  
   ghContainer �    	   �     cObjectName �    
   �     iStart  �       �     cAppService �       �     cASDivision (            cServerOperatingMode    D       <     cFields          X     iStartPage  �       |        pFlgEstDet           �        pGlosa           �  almtabla             <   �   �  �  �  �  �  �  �          )  5  6  7  9  ;  <  =  A  B  E  F  G  H  J  L  N  P  Q  R  U  W  X  Z  [  \  ]  ^  d  f  l  n  p  q  w  x  y  z  }  ~  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  	  {	  |	  	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
   
  !
  "
  #
  $
  %
  &
  '
  (
  )
  *
  +
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
  Z  f  �  �  �  �  �  �  �  �  �  �  �  �    .  0  E  �  �  �                     <  P  �      
    �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  L  s  �  �  �  �  �          	  
                  �� $ C:\Progress\OpenEdge\src\adm2\dialogmn.i �  f!  C:\Progress\OpenEdge\src\adm2\containr.i �  � # %C:\Progress\OpenEdge\src\adm2\custom\containrcustom.i       ��  C:\Progress\OpenEdge\src\adm2\visual.i   d  # " %C:\Progress\OpenEdge\src\adm2\custom\visualcustom.i  �  �<  C:\Progress\OpenEdge\src\adm2\appserver.i    �  �� ! %C:\Progress\OpenEdge\src\adm2\custom\appservercustom.i     I�  C:\Progress\OpenEdge\src\adm2\smart.i    T  Ds   C:\Progress\OpenEdge\gui\fn  �  tw  %C:\Progress\OpenEdge\src\adm2\custom\smartcustom.i   �  Q.  C:\Progress\OpenEdge\gui\set �  ��  C:\Progress\OpenEdge\src\adm2\cntnprop.i   ��  %C:\Progress\OpenEdge\src\adm2\custom\cntnpropcustom.i    L  P  %C:\Progress\OpenEdge\src\adm2\custom\cntnprtocustom.i    �  F>  C:\Progress\OpenEdge\src\adm2\visprop.i  �  �I  %C:\Progress\OpenEdge\src\adm2\custom\vispropcustom.i   ��  %C:\Progress\OpenEdge\src\adm2\custom\visprtocustom.i H  �l 
 C:\Progress\OpenEdge\src\adm2\appsprop.i �  ɏ  %C:\Progress\OpenEdge\src\adm2\custom\appspropcustom.i    �  V  %C:\Progress\OpenEdge\src\adm2\custom\appsprtocustom.i       i$  C:\Progress\OpenEdge\src\adm2\smrtprop.i D  �j  C:\Progress\OpenEdge\gui\get x  �  %C:\Progress\OpenEdge\src\adm2\custom\smrtpropcustom.i    �  ��  %C:\Progress\OpenEdge\src\adm2\custom\smrtprtocustom.i    �  ��  C:\Progress\OpenEdge\src\adm2\smrtprto.i (  Su  C:\Progress\OpenEdge\src\adm2\globals.i  \  M�  %C:\Progress\OpenEdge\src\adm2\custom\globalscustom.i �  )a  %C:\Progress\OpenEdge\src\adm2\custom\smartdefscustom.i   �  �  C:\Progress\OpenEdge\src\adm2\appsprto.i   ��  %C:\Progress\OpenEdge\src\adm2\custom\appserverdefscustom.i   H  �X 	 C:\Progress\OpenEdge\src\adm2\visprto.i  �  !�  %C:\Progress\OpenEdge\src\adm2\custom\visualdefscustom.i  �  n�  C:\Progress\OpenEdge\src\adm2\cntnprto.i   ;  %C:\Progress\OpenEdge\src\adm2\custom\containrdefscustom.i    <  ~�  C:\Progress\OpenEdge\src\adm2\widgetprto.i   �  e�  !C:\Progress\OpenEdge\gui\adecomm\appserv.i   �  �   d:\newsie\on_in_co\APLIC\logis\d-motivo-demora.w             0     �  $   @  �   �      P  �   �     `     k     p  �   f     �     D     �  �   <     �     �  #   �  �   �     �     �      �  �   �     �     �      �  �   �           �        r   �        n   �     0     3  "   @  i   .     P          `  P   �     p  �   �     �     �  !   �  �   �     �     k     �  �   j     �     H     �  �   F     �     $     �  g   
           �       O   �        �   ]     0     [      @  �   +     P     �     `  �   �     p     �     �  �   �     �     �     �  �   �     �     `     �  �   _     �     =     �  �   ,     �     
        �             �        }   �     0     �     @     ;     P     �     `     �     p  7   c     �  �   Z     �  O   L     �     ;     �     �
     �  �   �
     �  �   �
     �  O   �
     �     }
            /
        �   

         x   
  
   0   M   �	     @      �	     P      �	     `   a   y	  
   p   �  X	     �      9	     �   �  	     �   O   �     �      �     �      �     �   �   �     �      �     �      �      !  x   �     !     �      !     T     0!     P     @!     <     P!     #     `!  Q     
   p!     �     �!     �  
   �!     m     �!     S  
   �!  f   (     �!     �  	   �!  "   �     �!     o     �!     N      "  Z   �     "           "     �     0"     �     @"     �     P"     b     `"  ,   �       p"     E      �"  	   "       �"     	      