	��V�5�a�4  8��                                              9� 34B0010Autf-8 MAIN d:\newsie\on_in_co\APLIC\alm\walmacen.w,, PROCEDURE exitObject,, PROCEDURE enable_UI,, PROCEDURE disable_UI,, PROCEDURE adm-create-objects,, PROCEDURE start-super-proc,,INPUT pcProcName CHARACTER PROCEDURE adm-clone-props,, PROCEDURE toggleData,,INPUT plEnabled LOGICAL PROCEDURE showMessageProcedure,,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL PROCEDURE returnFocus,,INPUT hTarget HANDLE PROCEDURE repositionObject,,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL PROCEDURE removeLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE removeAllLinks,, PROCEDURE modifyUserLinks,,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE PROCEDURE modifyListProperty,,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER PROCEDURE hideObject,, PROCEDURE editInstanceProperties,, PROCEDURE displayLinks,, PROCEDURE createControls,, PROCEDURE changeCursor,,INPUT pcCursor CHARACTER PROCEDURE applyEntry,,INPUT pcField CHARACTER PROCEDURE adjustTabOrder,,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER PROCEDURE addMessage,,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER PROCEDURE addLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE unbindServer,,INPUT pcMode CHARACTER PROCEDURE startServerObject,, PROCEDURE runServerObject,,INPUT phAppService HANDLE PROCEDURE restartServerObject,, PROCEDURE initializeServerObject,, PROCEDURE disconnectObject,, PROCEDURE destroyServerObject,, PROCEDURE bindServer,, PROCEDURE processAction,,INPUT pcAction CHARACTER PROCEDURE enableObject,, PROCEDURE disableObject,, PROCEDURE applyLayout,, PROCEDURE viewPage,,INPUT piPageNum INTEGER PROCEDURE viewObject,, PROCEDURE toolbar,,INPUT pcValue CHARACTER PROCEDURE selectPage,,INPUT piPageNum INTEGER PROCEDURE removePageNTarget,,INPUT phTarget HANDLE,INPUT piPage INTEGER PROCEDURE passThrough,,INPUT pcLinkName CHARACTER,INPUT pcArgument CHARACTER PROCEDURE notifyPage,,INPUT pcProc CHARACTER PROCEDURE initPages,,INPUT pcPageList CHARACTER PROCEDURE initializeVisualContainer,, PROCEDURE initializeObject,, PROCEDURE hidePage,,INPUT piPageNum INTEGER PROCEDURE destroyObject,, PROCEDURE deletePage,,INPUT piPageNum INTEGER PROCEDURE createObjects,, PROCEDURE constructObject,,INPUT pcProcName CHARACTER,INPUT phParent HANDLE,INPUT pcPropList CHARACTER,OUTPUT phObject HANDLE PROCEDURE confirmExit,,INPUT-OUTPUT plCancel LOGICAL PROCEDURE changePage,, PROCEDURE assignPageProperty,,INPUT pcProp CHARACTER,INPUT pcValue CHARACTER FUNCTION Signature,CHARACTER,INPUT pcName CHARACTER FUNCTION showmessage,LOGICAL,INPUT pcMessage CHARACTER FUNCTION setUserProperty,LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION setUIBMode,LOGICAL,INPUT pcMode CHARACTER FUNCTION setTranslatableProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setSupportedLinks,LOGICAL,INPUT pcLinkList CHARACTER FUNCTION setRunAttribute,LOGICAL,INPUT cRunAttribute CHARACTER FUNCTION setPhysicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setPhysicalObjectName,LOGICAL,INPUT cTemp CHARACTER FUNCTION setPassThroughLinks,LOGICAL,INPUT pcLinks CHARACTER FUNCTION setParentDataKey,LOGICAL,INPUT cParentDataKey CHARACTER FUNCTION setObjectVersion,LOGICAL,INPUT cObjectVersion CHARACTER FUNCTION setObjectParent,LOGICAL,INPUT phParent HANDLE FUNCTION setObjectName,LOGICAL,INPUT pcName CHARACTER FUNCTION setLogicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setLogicalObjectName,LOGICAL,INPUT c CHARACTER FUNCTION setInstanceProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setDynamicObject,LOGICAL,INPUT lTemp LOGICAL FUNCTION setDesignDataObject,LOGICAL,INPUT pcDataObject CHARACTER FUNCTION setDBAware,LOGICAL,INPUT lAware LOGICAL FUNCTION setDataTargetEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setDataTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setDataSourceNames,LOGICAL,INPUT pcSourceNames CHARACTER FUNCTION setDataSourceEvents,LOGICAL,INPUT pcEventsList CHARACTER FUNCTION setDataSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDataLinksEnabled,LOGICAL,INPUT lDataLinksEnabled LOGICAL FUNCTION setContainerSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setContainerSource,LOGICAL,INPUT phObject HANDLE FUNCTION setContainerHidden,LOGICAL,INPUT plHidden LOGICAL FUNCTION setChildDataKey,LOGICAL,INPUT cChildDataKey CHARACTER FUNCTION reviewMessages,CHARACTER, FUNCTION propertyType,CHARACTER,INPUT pcPropName CHARACTER FUNCTION messageNumber,CHARACTER,INPUT piMessage INTEGER FUNCTION mappedEntry,CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER FUNCTION linkProperty,CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER FUNCTION linkHandles,CHARACTER,INPUT pcLink CHARACTER FUNCTION instancePropertyList,CHARACTER,INPUT pcPropList CHARACTER FUNCTION getUserProperty,CHARACTER,INPUT pcPropName CHARACTER FUNCTION getUIBMode,CHARACTER, FUNCTION getTranslatableProperties,CHARACTER, FUNCTION getSupportedLinks,CHARACTER, FUNCTION getRunAttribute,CHARACTER, FUNCTION getQueryObject,LOGICAL, FUNCTION getPropertyDialog,CHARACTER, FUNCTION getPhysicalVersion,CHARACTER, FUNCTION getPhysicalObjectName,CHARACTER, FUNCTION getPassThroughLinks,CHARACTER, FUNCTION getParentDataKey,CHARACTER, FUNCTION getObjectVersionNumber,CHARACTER, FUNCTION getObjectVersion,CHARACTER, FUNCTION getObjectParent,HANDLE, FUNCTION getObjectPage,INTEGER, FUNCTION getObjectName,CHARACTER, FUNCTION getObjectInitialized,LOGICAL, FUNCTION getObjectHidden,LOGICAL, FUNCTION getLogicalVersion,CHARACTER, FUNCTION getLogicalObjectName,CHARACTER, FUNCTION getInstanceProperties,CHARACTER, FUNCTION getDynamicObject,LOGICAL, FUNCTION getDesignDataObject,CHARACTER, FUNCTION getDBAware,LOGICAL, FUNCTION getDataTargetEvents,CHARACTER, FUNCTION getDataTarget,CHARACTER, FUNCTION getDataSourceNames,CHARACTER, FUNCTION getDataSourceEvents,CHARACTER, FUNCTION getDataSource,HANDLE, FUNCTION getDataLinksEnabled,LOGICAL, FUNCTION getContainerType,CHARACTER, FUNCTION getContainerSourceEvents,CHARACTER, FUNCTION getContainerSource,HANDLE, FUNCTION getContainerHidden,LOGICAL, FUNCTION getContainerHandle,HANDLE, FUNCTION getChildDataKey,CHARACTER, FUNCTION fetchMessages,CHARACTER, FUNCTION assignLinkProperty,LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION anyMessage,LOGICAL, FUNCTION setServerOperatingMode,LOGICAL,INPUT pcServerOperatingMode CHARACTER FUNCTION setServerFileName,LOGICAL,INPUT pcFileName CHARACTER FUNCTION setASUsePrompt,LOGICAL,INPUT plFlag LOGICAL FUNCTION setASInitializeOnRun,LOGICAL,INPUT plInitialize LOGICAL FUNCTION setASInfo,LOGICAL,INPUT pcInfo CHARACTER FUNCTION setASHandle,LOGICAL,INPUT phASHandle HANDLE FUNCTION setASDivision,LOGICAL,INPUT pcDivision CHARACTER FUNCTION setAppService,LOGICAL,INPUT pcAppService CHARACTER FUNCTION runServerProcedure,HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE FUNCTION getServerOperatingMode,CHARACTER, FUNCTION getServerFileName,CHARACTER, FUNCTION getASUsePrompt,LOGICAL, FUNCTION getASInitializeOnRun,LOGICAL, FUNCTION getASInfo,CHARACTER, FUNCTION getASHasStarted,LOGICAL, FUNCTION getASHandle,HANDLE, FUNCTION getAsDivision,CHARACTER, FUNCTION getASBound,LOGICAL, FUNCTION getAppService,CHARACTER, FUNCTION createUiEvents,LOGICAL, FUNCTION getObjectSecured,LOGICAL, FUNCTION getObjectTranslated,LOGICAL, FUNCTION setResizeVertical,LOGICAL,INPUT plResizeVertical LOGICAL FUNCTION setResizeHorizontal,LOGICAL,INPUT plResizeHorizontal LOGICAL FUNCTION setObjectLayout,LOGICAL,INPUT pcLayout CHARACTER FUNCTION setLayoutOptions,LOGICAL,INPUT pcOptions CHARACTER FUNCTION setHideOnInit,LOGICAL,INPUT plHide LOGICAL FUNCTION setDisableOnInit,LOGICAL,INPUT plDisable LOGICAL FUNCTION setDefaultLayout,LOGICAL,INPUT pcDefault CHARACTER FUNCTION setAllFieldNames,LOGICAL,INPUT pcValue CHARACTER FUNCTION setAllFieldHandles,LOGICAL,INPUT pcValue CHARACTER FUNCTION getResizeVertical,LOGICAL, FUNCTION getResizeHorizontal,LOGICAL, FUNCTION getWidth,DECIMAL, FUNCTION getRow,DECIMAL, FUNCTION getObjectLayout,CHARACTER, FUNCTION getObjectEnabled,LOGICAL, FUNCTION getLayoutVariable,CHARACTER, FUNCTION getLayoutOptions,CHARACTER, FUNCTION getHideOnInit,LOGICAL, FUNCTION getHeight,DECIMAL, FUNCTION getEnabledObjHdls,CHARACTER, FUNCTION getEnabledObjFlds,CHARACTER, FUNCTION getDisableOnInit,LOGICAL, FUNCTION getDefaultLayout,CHARACTER, FUNCTION getCol,DECIMAL, FUNCTION getAllFieldNames,CHARACTER, FUNCTION getAllFieldHandles,CHARACTER, FUNCTION setStatusArea,LOGICAL,INPUT plStatusArea LOGICAL FUNCTION getObjectType,character, FUNCTION setWindowTitleViewer,LOGICAL,INPUT phViewer HANDLE FUNCTION setWaitForObject,LOGICAL,INPUT phObject HANDLE FUNCTION setUpdateTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setUpdateSource,LOGICAL,INPUT pcSource CHARACTER FUNCTION setTopOnly,LOGICAL,INPUT plTopOnly LOGICAL FUNCTION setSdoForeignFields,LOGICAL,INPUT cSdoForeignFields CHARACTER FUNCTION setSavedContainerMode,LOGICAL,INPUT cSavedContainerMode CHARACTER FUNCTION setRunMultiple,LOGICAL,INPUT plMultiple LOGICAL FUNCTION setRunDOOptions,LOGICAL,INPUT pcOptions CHARACTER FUNCTION setRouterTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setReEnableDataLinks,LOGICAL,INPUT cReEnableDataLinks CHARACTER FUNCTION setPrimarySdoTarget,LOGICAL,INPUT hPrimarySdoTarget HANDLE FUNCTION setPageSource,LOGICAL,INPUT phObject HANDLE FUNCTION setPageNTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setOutMessageTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setNavigationTarget,LOGICAL,INPUT cTarget CHARACTER FUNCTION setNavigationSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setNavigationSource,LOGICAL,INPUT pcSource CHARACTER FUNCTION setMultiInstanceSupported,LOGICAL,INPUT lMultiInstanceSupported LOGICAL FUNCTION setMultiInstanceActivated,LOGICAL,INPUT lMultiInstanceActivated LOGICAL FUNCTION setInMessageTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setFilterSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDynamicSDOProcedure,LOGICAL,INPUT pcProc CHARACTER FUNCTION setDisabledAddModeTabs,LOGICAL,INPUT cDisabledAddModeTabs CHARACTER FUNCTION setCurrentPage,LOGICAL,INPUT iPage INTEGER FUNCTION setContainerTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setContainerMode,LOGICAL,INPUT cContainerMode CHARACTER FUNCTION setCallerWindow,LOGICAL,INPUT h HANDLE FUNCTION setCallerProcedure,LOGICAL,INPUT h HANDLE FUNCTION setCallerObject,LOGICAL,INPUT h HANDLE FUNCTION pageNTargets,CHARACTER,INPUT phTarget HANDLE,INPUT piPageNum INTEGER FUNCTION getStatusArea,LOGICAL, FUNCTION getWindowTitleViewer,HANDLE, FUNCTION getWaitForObject,HANDLE, FUNCTION getUpdateTarget,CHARACTER, FUNCTION getUpdateSource,CHARACTER, FUNCTION getTopOnly,LOGICAL, FUNCTION getSdoForeignFields,CHARACTER, FUNCTION getSavedContainerMode,CHARACTER, FUNCTION getRunMultiple,LOGICAL, FUNCTION getRunDOOptions,CHARACTER, FUNCTION getReEnableDataLinks,CHARACTER, FUNCTION getPrimarySdoTarget,HANDLE, FUNCTION getPageSource,HANDLE, FUNCTION getPageNTarget,CHARACTER, FUNCTION getOutMessageTarget,HANDLE, FUNCTION getNavigationTarget,HANDLE, FUNCTION getNavigationSourceEvents,CHARACTER, FUNCTION getNavigationSource,CHARACTER, FUNCTION getMultiInstanceSupported,LOGICAL, FUNCTION getMultiInstanceActivated,LOGICAL, FUNCTION getFilterSource,HANDLE, FUNCTION getDynamicSDOProcedure,CHARACTER, FUNCTION getDisabledAddModeTabs,CHARACTER, FUNCTION getCurrentPage,INTEGER, FUNCTION getContainerTargetEvents,CHARACTER, FUNCTION getContainerTarget,CHARACTER, FUNCTION getContainerMode,CHARACTER, FUNCTION getCallerWindow,HANDLE, FUNCTION getCallerProcedure,HANDLE, FUNCTION enablePagesInFolder,LOGICAL,INPUT pcPageInformation CHARACTER FUNCTION disablePagesInFolder,LOGICAL,INPUT pcPageInformation CHARACTER FUNCTION widgetValueList,CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION widgetValue,CHARACTER,INPUT pcName CHARACTER FUNCTION widgetIsTrue,LOGICAL,INPUT pcName CHARACTER FUNCTION widgetIsModified,LOGICAL,INPUT pcNameList CHARACTER FUNCTION widgetIsFocused,LOGICAL,INPUT pcName CHARACTER FUNCTION widgetIsBlank,LOGICAL,INPUT pcNameList CHARACTER FUNCTION widgetLongcharValue,LONGCHAR,INPUT pcName CHARACTER FUNCTION widgetHandle,HANDLE,INPUT pcName CHARACTER FUNCTION viewWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION toggleWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION resetWidgetValue,LOGICAL,INPUT pcNameList CHARACTER FUNCTION highlightWidget,LOGICAL,INPUT pcNameList CHARACTER,INPUT pcHighlightType CHARACTER FUNCTION hideWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION formattedWidgetValueList,CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION formattedWidgetValue,CHARACTER,INPUT pcName CHARACTER FUNCTION enableWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION enableRadioButton,LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER FUNCTION disableWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION disableRadioButton,LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER FUNCTION clearWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION blankWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION assignWidgetValueList,LOGICAL,INPUT pcNameList CHARACTER,INPUT pcValueList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION assignWidgetValue,LOGICAL,INPUT pcName CHARACTER,INPUT pcValue CHARACTER FUNCTION assignFocusedWidget,LOGICAL,INPUT pcName CHARACTER        �              H�              1z �  p�              <f              $    +   �> �  7   $C `  8   �F   A   �R |  B   T \  C   hU $  D           �V 0  ? �W u"  iSO8859-1                                                                           @    �                                       �                   �                         <   �    ��             �  �   �      �                                                         PROGRESS                         �           
    
                    �              �                                                                                                     
  H  �	      �  
    
                  �  x             4                                                                                          �	          
  �  �	      p  
    
                  \  $             �                                                                                          �	          
  �  �	        
    
                    �             �                                                                                          �	          
  L  �	      �  
    
                  �  |             8                                                                                          �	          
  �  �	      t  
    
                  `  (             �                                                                                          �	          
  �  �	         
    
                    �             �                                                                                          �	          
  P  �	      �  
    
                  �  �  	           <                                                                                          �	          
  �  
      x  
    
                  d  ,  
           �                                                                                          
          
  �   
      $                           �             �                                                                                           
            T  -
      �                        �  �             @                                                                                          -
             	  ;
      |  
    
                  h  0	             �                                                                                          ;
          
  �	  I
      (	  
    
                  	  �	             �	                                                                                          I
          
  X
  W
      �	  
    
                  �	  �
             D
                                                                                          W
          
    e
      �
                        l
  4             �
                                                                                          e
            �  u
      ,                          �             �                                                                                          u
            \  �
      �                        �  �             H                                                                                          �
                �
      �                        p                 �                                                                                          �
                          t�                                               x�          �  �  P �p                                        
             
             
             
             
             
             
                                         
                                                                                                                P   `   p   �   �   �   �   �   �   �   �           0  @  P  `  p      P   `   p   �   �   �   �   �   �   �   �          0  @  P  `  p    ��                                               �          ����                            undefined                                                               �           �   l                             �����                �0                    O   ����    e�          O   ����    R�          O   ����    ��      t       �   �              4   ����     /                                    3   ����       $     H  ���                       8      
                       � ߱        �  �      D       p
     7          assignFocusedWidget         �      �            LOGICAL,INPUT pcName CHARACTER  assignWidgetValue   �      �      $    $       LOGICAL,INPUT pcName CHARACTER,INPUT pcValue CHARACTER  assignWidgetValueList         \      �    6       LOGICAL,INPUT pcNameList CHARACTER,INPUT pcValueList CHARACTER,INPUT pcDelimiter CHARACTER  blankWidget t      �          L       LOGICAL,INPUT pcNameList CHARACTER  clearWidget �      @      l    X       LOGICAL,INPUT pcNameList CHARACTER  disableRadioButton  L      �      �    d       LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER    disableWidget   �            4    w       LOGICAL,INPUT pcNameList CHARACTER  enableRadioButton         X      �    �       LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER    enableWidget    l      �      �    �       LOGICAL,INPUT pcNameList CHARACTER  formattedWidgetValue    �             X  	  �       CHARACTER,INPUT pcName CHARACTER    formattedWidgetValueList    8      |      �  
  �       CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER    hideWidget  �      �      (   
 �       LOGICAL,INPUT pcNameList CHARACTER  highlightWidget       L      |    �       LOGICAL,INPUT pcNameList CHARACTER,INPUT pcHighlightType CHARACTER  resetWidgetValue    \      �      �    �       LOGICAL,INPUT pcNameList CHARACTER  toggleWidget    �            H    �       LOGICAL,INPUT pcNameList CHARACTER  viewWidget  (      l      �   
       LOGICAL,INPUT pcNameList CHARACTER  widgetHandle    x      �      �          HANDLE,INPUT pcName CHARACTER   widgetLongcharValue �            @    #      LONGCHAR,INPUT pcName CHARACTER widgetIsBlank          `      �    7      LOGICAL,INPUT pcNameList CHARACTER  widgetIsFocused p      �      �    E      LOGICAL,INPUT pcName CHARACTER  widgetIsModified    �      	      8	    U      LOGICAL,INPUT pcNameList CHARACTER  widgetIsTrue    	      \	      �	    f      LOGICAL,INPUT pcName CHARACTER  widgetValue l	      �	      �	    s      CHARACTER,INPUT pcName CHARACTER    widgetValueList �	      �	      ,
          CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER    �      �
  �
  P  d       4   ����d       o          �
                              �  �   NA  �   �  �   �  �      �      �     �         $    8    L  `  `  
`  t  $  �    �     �      $  0  |  ���                       �     
                    � ߱        ؁    _  �  @      �      4   �����                P                      ��                  `  i                  ��]                       `  �  �    b  l  |             4   ����       $  c  �  ���                       P  @         <              � ߱              f  �         �      4   �����      $  g  ,  ���                       �  @         �              � ߱        assignPageProperty                              �  �      ��                  �  �                $�^                    O   ����    e�          O   ����    R�          O   ����    ��            ��   T                             ��                  H           ��                            ����                            changePage                              @  (      ��                  �  �  X              �]                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            confirmExit                             @  (      ��                  �  �  X              8]                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  p           ��                            ����                            constructObject                             l  T      ��                  �  �  �              �]                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             �               �� 
  �             �  
             ��                 �               �� 
                   
         ��                            ����                            createObjects                                 �      ��                  �  �  (              �l\                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            deletePage                                �      ��                  �  �  (              pE]                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  @           ��                            ����                            destroyObject                               <  $      ��                      T              ��^                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hidePage                                <  $      ��                      T              t�^                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  l           ��                            ����                            initializeObject                                l  T      ��                    	  �              ��^                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeVisualContainer                               |  d      ��                      �              T/^                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initPages                               |  d      ��                      �              0^                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            notifyPage                              �  �      ��                      �              d.]                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            passThrough                             �  �      ��                      �              �U]                    O   ����    e�          O   ����    R�          O   ����    ��            ��   0             �               ��                  $           ��                            ����                            removePageNTarget                               $        ��                      <              ��\                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �             T  
             ��                  |           ��                            ����                            selectPage                              t  \      ��                     "  �              ,]                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            toolbar                             �  �      ��                  $  &  �              ��\                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            viewObject                              �   �       ��                  (  )  �               �M\                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            viewPage                                �!  �!      ��                  +  -  �!              �P\                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �!           ��                            ����                            disablePagesInFolder    
      X"      �"    �      LOGICAL,INPUT pcPageInformation CHARACTER   enablePagesInFolder p"      �"      �"           LOGICAL,INPUT pcPageInformation CHARACTER   getCallerProcedure  �"      #      P#          HANDLE, getCallerWindow 0#      X#      �#    '      HANDLE, getContainerMode    h#      �#      �#    7      CHARACTER,  getContainerTarget  �#      �#      $    H      CHARACTER,  getContainerTargetEvents    �#      $      L$    [      CHARACTER,  getCurrentPage  ,$      X$      �$    t      INTEGER,    getDisabledAddModeTabs  h$      �$      �$     �      CHARACTER,  getDynamicSDOProcedure  �$      �$      %  !  �      CHARACTER,  getFilterSource �$      %      L%  "  �      HANDLE, getMultiInstanceActivated   ,%      T%      �%  #  �      LOGICAL,    getMultiInstanceSupported   p%      �%      �%  $  �      LOGICAL,    getNavigationSource �%      �%      &  %  �      CHARACTER,  getNavigationSourceEvents   �%      $&      `&  &  	      CHARACTER,  getNavigationTarget @&      l&      �&  '  #      HANDLE, getOutMessageTarget �&      �&      �&  (  7      HANDLE, getPageNTarget  �&      �&      '  )  K      CHARACTER,  getPageSource   �&       '      P'  *  Z      HANDLE, getPrimarySdoTarget 0'      X'      �'  +  h      HANDLE, getReEnableDataLinks    l'      �'      �'  ,  |      CHARACTER,  getRunDOOptions �'      �'      (  -  �      CHARACTER,  getRunMultiple  �'      (      D(  .  �      LOGICAL,    getSavedContainerMode   $(      P(      �(  /  �      CHARACTER,  getSdoForeignFields h(      �(      �(  0  �      CHARACTER,  getTopOnly  �(      �(       )  1 
 �      LOGICAL,    getUpdateSource �(      )      <)  2  �      CHARACTER,  getUpdateTarget )      H)      x)  3  �      CHARACTER,  getWaitForObject    X)      �)      �)  4        HANDLE, getWindowTitleViewer    �)      �)      �)  5        HANDLE, getStatusArea   �)       *      0*  6  +      LOGICAL,    pageNTargets    *      <*      l*  7  9      CHARACTER,INPUT phTarget HANDLE,INPUT piPageNum INTEGER setCallerObject L*      �*      �*  8  F      LOGICAL,INPUT h HANDLE  setCallerProcedure  �*      �*       +  9  V      LOGICAL,INPUT h HANDLE  setCallerWindow  +      8+      h+  :  i      LOGICAL,INPUT h HANDLE  setContainerMode    H+      �+      �+  ;  y      LOGICAL,INPUT cContainerMode CHARACTER  setContainerTarget  �+      �+      ,  <  �      LOGICAL,INPUT pcObject CHARACTER    setCurrentPage  �+      4,      d,  =  �      LOGICAL,INPUT iPage INTEGER setDisabledAddModeTabs  D,      �,      �,  >  �      LOGICAL,INPUT cDisabledAddModeTabs CHARACTER    setDynamicSDOProcedure  �,      �,       -  ?  �      LOGICAL,INPUT pcProc CHARACTER  setFilterSource  -      @-      p-  @  �      LOGICAL,INPUT phObject HANDLE   setInMessageTarget  P-      �-      �-  A  �      LOGICAL,INPUT phObject HANDLE   setMultiInstanceActivated   �-      �-       .  B  �      LOGICAL,INPUT lMultiInstanceActivated LOGICAL   setMultiInstanceSupported    .      P.      �.  C        LOGICAL,INPUT lMultiInstanceSupported LOGICAL   setNavigationSource l.      �.      �.  D  1      LOGICAL,INPUT pcSource CHARACTER    setNavigationSourceEvents   �.      /      P/  E  E      LOGICAL,INPUT pcEvents CHARACTER    setNavigationTarget 0/      t/      �/  F  _      LOGICAL,INPUT cTarget CHARACTER setOutMessageTarget �/      �/      �/  G  s      LOGICAL,INPUT phObject HANDLE   setPageNTarget  �/      0      L0  H  �      LOGICAL,INPUT pcObject CHARACTER    setPageSource   ,0      p0      �0  I  �      LOGICAL,INPUT phObject HANDLE   setPrimarySdoTarget �0      �0      �0  J  �      LOGICAL,INPUT hPrimarySdoTarget HANDLE  setReEnableDataLinks    �0      1      T1  K  �      LOGICAL,INPUT cReEnableDataLinks CHARACTER  setRouterTarget 41      �1      �1  L  �      LOGICAL,INPUT phObject HANDLE   setRunDOOptions �1      �1       2  M  �      LOGICAL,INPUT pcOptions CHARACTER   setRunMultiple  �1      $2      T2  N  �      LOGICAL,INPUT plMultiple LOGICAL    setSavedContainerMode   42      x2      �2  O  �      LOGICAL,INPUT cSavedContainerMode CHARACTER setSdoForeignFields �2      �2      3  P        LOGICAL,INPUT cSdoForeignFields CHARACTER   setTopOnly  �2      <3      h3  Q 
 &      LOGICAL,INPUT plTopOnly LOGICAL setUpdateSource H3      �3      �3  R  1      LOGICAL,INPUT pcSource CHARACTER    setUpdateTarget �3      �3      4  S  A      LOGICAL,INPUT pcTarget CHARACTER    setWaitForObject    �3      04      d4  T  Q      LOGICAL,INPUT phObject HANDLE   setWindowTitleViewer    D4      �4      �4  U  b      LOGICAL,INPUT phViewer HANDLE   getObjectType   �4      �4      5  V  w      CHARACTER,  setStatusArea   �4      5      H5  W  �      LOGICAL,INPUT plStatusArea LOGICAL  applyLayout                             �5  �5      ��                  �  �  6              @�_                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disableObject                                7  �6      ��                  �  �  7              �_                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            enableObject                                8  �7      ��                  �  �  8              ��_                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                9  �8      ��                  �  �  $9              x:u                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            processAction                               :  �9      ��                  �  �  (:              p;u                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  @:           ��                            ����                            getAllFieldHandles  (5      �:      �:  X  �      CHARACTER,  getAllFieldNames    �:      �:      ;  Y  �      CHARACTER,  getCol  �:      (;      P;  Z  �      DECIMAL,    getDefaultLayout    0;      \;      �;  [  �      CHARACTER,  getDisableOnInit    p;      �;      �;  \  �      LOGICAL,    getEnabledObjFlds   �;      �;      <  ]  �      CHARACTER,  getEnabledObjHdls   �;      <      P<  ^  �      CHARACTER,  getHeight   0<      \<      �<  _ 	       DECIMAL,    getHideOnInit   h<      �<      �<  `        LOGICAL,    getLayoutOptions    �<      �<      =  a        CHARACTER,  getLayoutVariable   �<      =      D=  b  -      CHARACTER,  getObjectEnabled    $=      P=      �=  c  ?      LOGICAL,    getObjectLayout d=      �=      �=  d  P      CHARACTER,  getRow  �=      �=      �=  e  `      DECIMAL,    getWidth    �=       >      ,>  f  g      DECIMAL,    getResizeHorizontal >      8>      l>  g  p      LOGICAL,    getResizeVertical   L>      x>      �>  h  �      LOGICAL,    setAllFieldHandles  �>      �>      �>  i  �      LOGICAL,INPUT pcValue CHARACTER setAllFieldNames    �>      ?      @?  j  �      LOGICAL,INPUT pcValue CHARACTER setDefaultLayout     ?      `?      �?  k  �      LOGICAL,INPUT pcDefault CHARACTER   setDisableOnInit    t?      �?      �?  l  �      LOGICAL,INPUT plDisable LOGICAL setHideOnInit   �?      @      <@  m  �      LOGICAL,INPUT plHide LOGICAL    setLayoutOptions    @      \@      �@  n  �      LOGICAL,INPUT pcOptions CHARACTER   setObjectLayout p@      �@      �@  o  �      LOGICAL,INPUT pcLayout CHARACTER    setResizeHorizontal �@      A      <A  p        LOGICAL,INPUT plResizeHorizontal LOGICAL    setResizeVertical   A      hA      �A  q        LOGICAL,INPUT plResizeVertical LOGICAL  getObjectTranslated |A      �A      �A  r  1      LOGICAL,    getObjectSecured    �A      B      8B  s  E      LOGICAL,    createUiEvents  B      DB      tB  t  V      LOGICAL,    bindServer                              C  �B      ��                  �  �  (C              l'v                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               D  �C      ��                  �  �  ,D              (v                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyServerObject                             E  E      ��                  �  �  4E              h:�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disconnectObject                                $F  F      ��                  �  �  <F              �:�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              0G  G      ��                  �  �  HG              �;�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            restartServerObject                             8H   H      ��                  �  �  PH              �N�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            runServerObject                             <I  $I      ��                  �  �  TI              @O�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 lI  
         ��                            ����                            startServerObject                               lJ  TJ      ��                  �  �  �J              �K�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            unbindServer                                pK  XK      ��                  �  �  �K              �(�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �K           ��                            ����                            getAppService   TB      L      8L  u  e      CHARACTER,  getASBound  L      DL      pL  v 
 s      LOGICAL,    getAsDivision   PL      |L      �L  w  ~      CHARACTER,  getASHandle �L      �L      �L  x  �      HANDLE, getASHasStarted �L      �L      M  y  �      LOGICAL,    getASInfo   �L      (M      TM  z 	 �      CHARACTER,  getASInitializeOnRun    4M      `M      �M  {  �      LOGICAL,    getASUsePrompt  xM      �M      �M  |  �      LOGICAL,    getServerFileName   �M      �M      N  }  �      CHARACTER,  getServerOperatingMode  �M       N      XN  ~  �      CHARACTER,  runServerProcedure  8N      dN      �N    �      HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE   setAppService   xN      �N      O  �  	      LOGICAL,INPUT pcAppService CHARACTER    setASDivision   �N      4O      dO  �   	      LOGICAL,INPUT pcDivision CHARACTER  setASHandle DO      �O      �O  �  .	      LOGICAL,INPUT phASHandle HANDLE setASInfo   �O      �O       P  � 	 :	      LOGICAL,INPUT pcInfo CHARACTER  setASInitializeOnRun    �O       P      XP  �  D	      LOGICAL,INPUT plInitialize LOGICAL  setASUsePrompt  8P      |P      �P  �  Y	      LOGICAL,INPUT plFlag LOGICAL    setServerFileName   �P      �P       Q  �  h	      LOGICAL,INPUT pcFileName CHARACTER  setServerOperatingMode  �P      $Q      \Q  �  z	      LOGICAL,INPUT pcServerOperatingMode CHARACTER   addLink                             R   R      ��                  w  {  0R              �g                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  |R             HR  
             ��   �R             pR               �� 
                 �R  
         ��                            ����                            addMessage                              �S  xS      ��                  }  �  �S              @v                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �S             �S               ��   T             �S               ��                  T           ��                            ����                            adjustTabOrder                              U  �T      ��                  �  �  $U              �}                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  pU             <U  
             �� 
  �U             dU  
             ��                  �U           ��                            ����                            applyEntry                              �V  lV      ��                  �  �  �V              h�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �V           ��                            ����                            changeCursor                                �W  �W      ��                  �  �  �W              ̉                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �W           ��                            ����                            createControls                              �X  �X      ��                  �  �  �X              ,�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �Y  �Y      ��                  �  �  �Y              ؎                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            displayLinks                                �Z  �Z      ��                  �  �  �Z               �                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            editInstanceProperties                              �[  �[      ��                  �  �  \              В                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            exitObject                              �\  �\      ��                  �  �  ]              Е                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hideObject                              �]  �]      ��                  �  �  ^              h�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �^  �^      ��                  �  �  _              �                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            modifyListProperty                               `  �_      ��                  �  �  `              l�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  d`             0`  
             ��   �`             X`               ��   �`             �`               ��                  �`           ��                            ����                            modifyUserLinks                             �a  �a      ��                  �  �  �a              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��   b             �a               ��   0b             �a               �� 
                 $b  
         ��                            ����                            removeAllLinks                               c  c      ��                  �  �  8c              d�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            removeLink                               d  d      ��                  �  �  8d              @�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �d             Pd  
             ��   �d             xd               �� 
                 �d  
         ��                            ����                            repositionObject                                �e  �e      ��                  �  �  �e              4�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   f             �e               ��                  �e           ��                            ����                            returnFocus                             �f  �f      ��                  �  �  g              4�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                  g  
         ��                            ����                            showMessageProcedure                                $h  h      ��                  �  �  <h              h�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �h             Th               ��                  |h           ��                            ����                            toggleData                              ti  \i      ��                  �  �  �i              T�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �i           ��                            ����                            viewObject                              �j  �j      ��                  �  �  �j              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            anyMessage  <Q      k      8k  � 
 �
      LOGICAL,    assignLinkProperty  k      Dk      xk  �  �
      LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER   fetchMessages   Xk      �k       l  �  �
      CHARACTER,  getChildDataKey �k      l      <l  �        CHARACTER,  getContainerHandle  l      Hl      |l  �        HANDLE, getContainerHidden  \l      �l      �l  �  .      LOGICAL,    getContainerSource  �l      �l      �l  �  A      HANDLE, getContainerSourceEvents    �l       m      <m  �  T      CHARACTER,  getContainerType    m      Hm      |m  �  m      CHARACTER,  getDataLinksEnabled \m      �m      �m  �  ~      LOGICAL,    getDataSource   �m      �m      �m  �  �      HANDLE, getDataSourceEvents �m       n      4n  �  �      CHARACTER,  getDataSourceNames  n      @n      tn  �  �      CHARACTER,  getDataTarget   Tn      �n      �n  �  �      CHARACTER,  getDataTargetEvents �n      �n      �n  �  �      CHARACTER,  getDBAware  �n      �n      (o  � 
 �      LOGICAL,    getDesignDataObject o      4o      ho  �  �      CHARACTER,  getDynamicObject    Ho      to      �o  �        LOGICAL,    getInstanceProperties   �o      �o      �o  �        CHARACTER,  getLogicalObjectName    �o      �o      0p  �  /      CHARACTER,  getLogicalVersion   p      <p      pp  �  D      CHARACTER,  getObjectHidden Pp      |p      �p  �  V      LOGICAL,    getObjectInitialized    �p      �p      �p  �  f      LOGICAL,    getObjectName   �p      �p      ,q  �  {      CHARACTER,  getObjectPage   q      8q      hq  �  �      INTEGER,    getObjectParent Hq      tq      �q  �  �      HANDLE, getObjectVersion    �q      �q      �q  �  �      CHARACTER,  getObjectVersionNumber  �q      �q      $r  �  �      CHARACTER,  getParentDataKey    r      0r      dr  �  �      CHARACTER,  getPassThroughLinks Dr      pr      �r  �  �      CHARACTER,  getPhysicalObjectName   �r      �r      �r  �  �      CHARACTER,  getPhysicalVersion  �r      �r      (s  �  
      CHARACTER,  getPropertyDialog   s      4s      hs  �        CHARACTER,  getQueryObject  Hs      ts      �s  �  /      LOGICAL,    getRunAttribute �s      �s      �s  �  >      CHARACTER,  getSupportedLinks   �s      �s       t  �  N      CHARACTER,  getTranslatableProperties    t      ,t      ht  �  `      CHARACTER,  getUIBMode  Ht      tt      �t  � 
 z      CHARACTER,  getUserProperty �t      �t      �t  �  �      CHARACTER,INPUT pcPropName CHARACTER    instancePropertyList    �t      u      <u  �  �      CHARACTER,INPUT pcPropList CHARACTER    linkHandles u      du      �u  �  �      CHARACTER,INPUT pcLink CHARACTER    linkProperty    pu      �u      �u  �  �      CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER mappedEntry �u       v      Lv  �  �      CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER  messageNumber   ,v      �v      �v  �  �      CHARACTER,INPUT piMessage INTEGER   propertyType    �v      w      <w  �  �      CHARACTER,INPUT pcPropName CHARACTER    reviewMessages  w      dw      �w  �  �      CHARACTER,  setChildDataKey tw      �w      �w  �  �      LOGICAL,INPUT cChildDataKey CHARACTER   setContainerHidden  �w      �w      ,x  �  	      LOGICAL,INPUT plHidden LOGICAL  setContainerSource  x      Lx      �x  �        LOGICAL,INPUT phObject HANDLE   setContainerSourceEvents    `x      �x      �x  �  /      LOGICAL,INPUT pcEvents CHARACTER    setDataLinksEnabled �x       y      4y  �  H      LOGICAL,INPUT lDataLinksEnabled LOGICAL setDataSource   y      \y      �y  �  \      LOGICAL,INPUT phObject HANDLE   setDataSourceEvents ly      �y      �y  �  j      LOGICAL,INPUT pcEventsList CHARACTER    setDataSourceNames  �y      z      <z  �  ~      LOGICAL,INPUT pcSourceNames CHARACTER   setDataTarget   z      dz      �z  �  �      LOGICAL,INPUT pcTarget CHARACTER    setDataTargetEvents tz      �z      �z  �  �      LOGICAL,INPUT pcEvents CHARACTER    setDBAware  �z      {      <{  � 
 �      LOGICAL,INPUT lAware LOGICAL    setDesignDataObject {      \{      �{  �  �      LOGICAL,INPUT pcDataObject CHARACTER    setDynamicObject    p{      �{      �{  �  �      LOGICAL,INPUT lTemp LOGICAL setInstanceProperties   �{      |      @|  �  �      LOGICAL,INPUT pcPropList CHARACTER  setLogicalObjectName     |      d|      �|  �  �      LOGICAL,INPUT c CHARACTER   setLogicalVersion   ||      �|      �|  �        LOGICAL,INPUT cVersion CHARACTER    setObjectName   �|      }      @}  �         LOGICAL,INPUT pcName CHARACTER  setObjectParent  }      `}      �}  �  .      LOGICAL,INPUT phParent HANDLE   setObjectVersion    p}      �}      �}  �  >      LOGICAL,INPUT cObjectVersion CHARACTER  setParentDataKey    �}      ~      @~  �  O      LOGICAL,INPUT cParentDataKey CHARACTER  setPassThroughLinks  ~      h~      �~  �  `      LOGICAL,INPUT pcLinks CHARACTER setPhysicalObjectName   |~      �~      �~  �  t      LOGICAL,INPUT cTemp CHARACTER   setPhysicalVersion  �~            H  �  �      LOGICAL,INPUT cVersion CHARACTER    setRunAttribute (      l      �  �  �      LOGICAL,INPUT cRunAttribute CHARACTER   setSupportedLinks   |      �      �  �  �      LOGICAL,INPUT pcLinkList CHARACTER  setTranslatableProperties   �      �      X�  �  �      LOGICAL,INPUT pcPropList CHARACTER  setUIBMode  8�      |�      ��  � 
 �      LOGICAL,INPUT pcMode CHARACTER  setUserProperty ��      Ȁ      ��  �  �      LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER  showmessage ؀      8�      d�  �  �      LOGICAL,INPUT pcMessage CHARACTER   Signature   D�      ��      ��  � 	        CHARACTER,INPUT pcName CHARACTER    ��    �  �  p�            4   ����                ��                      ��                  �                    �                       �  �        �  ��  �      (      4   ����(                (�                      ��                  �                    H                       �  ��  (�    �  D�  ��      <      4   ����<                Ѓ                      ��                                      �                         T�                                           �     
  
       
           � ߱        T�  $    ��  ���                           $    ��  ���                       $                         � ߱        ��      Ȅ  D�      4      4   ����4                T�                      ��                    �                  �                         ؄  ��  o      	   ,                                 ��  $     ��  ���                       �  @         �              � ߱        �  �     �      �  �     <      �  �      �      0�  �   "  $      D�  �   $  �      X�  �   &        l�  �   '  �      ��  �   (  �      ��  �   +  8      ��  �   -  �      ��  �   .  (	      І  �   0  �	      �  �   1   
      ��  �   2  \
      �  �   3  �
       �  �   4  L      4�  �   :  �      H�  �   <  �      \�  �   B  8      p�  �   D  �      ��  �   F         ��  �   G  �      ��  �   M        ��  �   N  �      ԇ  �   O        �  �   P  |      ��  �   S  �      �  �   T  ,      $�  �   V  �      8�  �   W  �      L�  �   Y  P      `�  �   Z  �      t�  �   [  �      ��  �   \        ��  �   ]  @      ��  �   ^  �      Ĉ  �   _  �      ؈  �   a  4      �  �   b  p       �  �   c  �      �  �   e  �      (�  �   f  $      <�  �   g  `      P�  �   h  �          �   i  �                      |�          �  Љ      ��                  	  1	   �              �                    O   ����    e�          O   ����    R�          O   ����    ��      H     
                �                     �                         � ߱        ��  $ 	  �  ���                           O   /	  ��  ��                 �          �  �    �                                             ��                            ����                                �4      d�      ��     6     �                      V �  w                     x�    Q	  ԋ  P�             4   ����                 `�                      ��                  R	  �	                  \0�                       R	  �  t�  �   U	  �      ��  �   V	  �      ��  �   W	  p      ��  �   X	  �      Č  �   Y	  h      ،  �   Z	  �      �  �   [	  X       �  �   \	  �      �  �   ]	  P      (�  �   ^	  �      <�  �   _	  @      P�  �   `	  �      d�  �   a	  8          �   b	  �      P�    �	  ��  �      $      4   ����$                 �                      ��                  �	  r
                  �1�                       �	  ��  4�  �   �	  �      H�  �   �	  �      \�  �   �	  l      p�  �   �	  �      ��  �   �	  \       ��  �   �	  �       ��  �   �	  L!      ��  �   �	  �!      Ԏ  �   �	  4"      �  �   �	  �"      ��  �   �	  $#      �  �   �	  �#      $�  �   �	  $      8�  �   �	  �$      L�  �   �	  %      `�  �   �	  �%      t�  �   �	  �%      ��  �   �	  x&      ��  �   �	  �&      ��  �   �	  p'      ď  �   �	  �'      ؏  �   �	  h(      �  �   �	  �(       �  �   �	  `)      �  �   �	  �)      (�  �   �	  X*      <�  �    
  �*          �   
  P+      l�    ~
  l�  �      �+      4   �����+                ��                      ��                  
  0                  ��u                       
  |�  �  �   �
  ,       �  �   �
  �,      4�  �   �
  -      H�  �   �
  �-      \�  �   �
  �-      p�  �   �
  l.      ��  �   �
  �.      ��  �   �
  /      ��  �   �
  �/      ��  �   �
  �/      ԑ  �   �
  0      �  �   �
  |0      ��  �   �
  �0      �  �   �
  l1      $�  �   �
  �1      8�  �   �
  T2      L�  �   �
  �2      `�  �   �
  D3      t�  �   �
  �3      ��  �   �
  �3      ��  �   �
  p4      ��  �   �
  �4      Ē  �   �
  X5      ؒ  �   �
  �5      �  �   �
  �5       �  �   �
  L6      �  �   �
  �6      (�  �   �
  �6      <�  �   �
   7      P�  �   �
  <7      d�  �   �
  x7      x�  �   �
  �7      ��  �   �
  �7      ��  �   �
  d8      ��  �   �
  �8      ȓ  �   �
  �8      ܓ  �   �
  9      �  �   �
  T9      �  �   �
  �9      �  �   �
  �9      ,�  �   �
  :      @�  �   �
  |:      T�  �   �
  �:      h�  �   �
  d;      |�  �   �
  �;      ��  �   �
  T<      ��  �   �
  �<      ��  �   �
  L=      ̔  �   �
  �=      ��  �   �
  D>      ��  �   �
  �>      �  �   �
  �>      �  �   �
  x?      0�  �   �
  �?      D�  �   �
  �?      X�  �   �
  ,@          �   �
  �@      ĕ  $  <  ��  ���                       A     
                    � ߱        \�    u  ��  �      A      4   ����A      /   v  �     ,�                          3   ����$A            L�                      3   ����DA  ��      x�  ��  ��  `A      4   ����`A  	              �                      ��             	     �                    XE                       �  ��  �  �   �  �A      p�  $  �  D�  ���                       �A     
  
       
           � ߱        ��  �   �  B      ܗ  $   �  ��  ���                       4B  @          B              � ߱        ��  $  �  �  ���                       �B                         � ߱        �B     
                xC                     �D  @        
 �D              � ߱        (�  V   �  4�  ���                        �D                     E                     DE                         � ߱        ��  $  �  Ę  ���                       F     
                �F                     �G  @        
 �G              � ߱        H�  V   �  T�  ���                        �G     
                XH                     �I  @        
 hI              � ߱            V   �  �  ���                        
              ��                      ��             
       �                  G                         t�  �I     
                0J                     �K  @        
 @K          �K  @        
 �K          DL  @        
 L          �L  @        
 dL              � ߱            V     �  ���                        adm-clone-props \�  ԛ              �     7     `                          \  J                     start-super-proc    �  @�  �           �     8                                  k                     H�    �  ̜  ܜ      0P      4   ����0P      /   �  �     �                          3   ����@P            8�                      3   ����`P  ��  $  �  t�  ���                       �P                         � ߱        \�    �  ��  8�  ؞  �P      4   �����P                ��                      ��                  �  �                  \B                       �  ̝  �P                     �P                     �P                         � ߱            $  �  H�  ���                             �  ��  0�      �P      4   �����P  Q                         � ߱            $  �  �  ���                       X�    �  x�  ��  ��  $Q      4   ����$Q      $  �  ��  ���                       DQ                         � ߱            �     XQ      �Q     
                R                     dS  @        
 $S              � ߱        ��  V   &  ��  ���                        ��  �   Y  pS      0�    �  ��  Ġ      �S      4   �����S      /   �  �      �                          3   �����S             �                      3   �����S  �    C  L�  ȡ      �S      4   �����S                ء                      ��                  D  G                  �:                       D  \�      g   E  �         \���                           ��          ��  p�      ��                  F      ��              �:                    O   ����    e�          O   ����    R�          O   ����    ��          /  F  �     ��  $T                      3   ����T  $�     
   �                      3   ����0T         
   D�                      3   ����8T    ��                              ��        �                  ����                                        �              9      T�                      g                               �  g   I  (�          \�	��                           �          ��  ��      ��                  I  K  ؤ              �;                    O   ����    e�          O   ����    R�          O   ����    ��          /  J  �     ,�  \T                      3   ����@T            L�                      3   ����dT    ��                              ��        �                  ����                                        <�              :      \�                      g                                �  g   M  0�          \�	ħ                           ��          Ȧ  ��      ��                  M  O  �              ,<                    O   ����    e�          O   ����    R�          O   ����    ��          /  N  $�     4�  �T                      3   �����T            T�                      3   �����T    ��                              ��        �                  ����                                        D�              ;      d�                      g                               ��    f  <�  ��      �T      4   �����T                Ȩ                      ��                  g  �                  ��                       g  L�  4�  /   h  ��     �                          3   �����T            $�                      3   �����T  0�  /  j  `�     p�  ,U                      3   ����U  ��     
   ��                      3   ����4U  Щ        ��                      3   ����<U   �        �                      3   ����PU             �                      3   ����tU  X�    r  L�  \�      �U      4   �����U      /  x  ��     ��   V                      3   ���� V  Ȫ     
   ��                      3   ����(V  ��        �                      3   ����0V  (�        �                      3   ����DV            H�                      3   ����hV        ~  t�  ��      �V      4   �����V      /  �  ��     ��  �V                      3   �����V  �     
   �                      3   �����V   �        �                      3   �����V  P�        @�                      3   ���� W            p�                      3   ����W  @�    �  ��  �      @W      4   ����@W                (�                      ��                  �  �                  ��                       �  ��      g   �  @�         \��        PW                  �          ح  ��      ��                  �      �              �                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  4�     D�  tW                      3   ����\W  t�     
   d�                      3   �����W         
   ��                      3   �����W    ��                            ����                                        T�              <      ��                      g                               د     �  �W                                     �W     
                 X                     pY  @        
 0Y              � ߱        h�  V      t�  ���                        �Y     
                 Z                     P[  @        
 [              � ߱        ��  V   '  �  ���                        �    V  ��  ��      d[      4   ����d[      $   W  �  ���                       �[  @         �[              � ߱        �  g   d  0�         \���        �[  \���        �[                  �          ܱ  ı      ��                  e  j  ��              (�                    O   ����    e�          O   ����    R�          O   ����    ��            i  (�  8�      �[      4   �����[      O  i  ������  \    ��                            ����                                        X�              =      P�                      g                               ��  g   q  �         \6<�         \                  ̳          ��  ��      ��                  r  w  ��              Č                    O   ����    e�          O   ����    R�          O   ����    ��      �    u  $\  }          O  v  ������  8\    ��                            ����                                        �              >      ��                      g                                     �  ��  0�      L\      4   ����L\                ��                      ��                  �  �                  $�]                       �  Ĵ  \\  @                     �\  @         t\          �\  @         �\              � ߱        е  $   �  @�  ���                       ̷  g   �  �         \np�      }                      ��          ��  h�      ��                  �  �  ��              _                    O   ����    e�          O   ����    R�          O   ����    ��      �  /  �  ܶ                                 3   �����\        �  �  �      �\      4   �����\      O  �  ������  ]    ��                            ����                                        ��              ?      0�                      g                               ��  g   �  �         \!D�          ]                  ظ          |�  d�      ��                  �  �  ��              �_                    O   ����    e�          O   ����    R�          O   ����    ��      ,]  @                         � ߱            $  �  ��  ���                         ��                            ����                                        ��              @      �                      g                               ܹ  /   �  ̹                                 3   ����4]        �  ��  t�      P]      4   ����P]                �                      ��                  �  �                  X_                       �  �                0�          �   �      ��                 �  �                  ��                       �  ��      O   �    ��          O   �    ��      l�  /   �  \�                                 3   ����h]        �  ��  ��      �]      4   �����]      k   �  ��              }       n        �   adm-create-objects  T�  ̻              P     A     �                          �  B"                     disable_UI  �  <�                      B      <                              U"  
                   enable_UI   H�  ��                      C      �                               `"  	                   exitObject  ��  �                      D      �                               j"  
                    �  �     �������  �              ��  ��      toggleData  ,INPUT plEnabled LOGICAL    ��  ܽ  ��      showMessageProcedure    ,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL  ̽  8�  D�      returnFocus ,INPUT hTarget HANDLE   (�  l�  ��      repositionObject    ,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL    \�  ��  Ⱦ      removeLink  ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE ��  �  ,�      removeAllLinks  ,   �  @�  P�      modifyUserLinks ,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE 0�  ��  ��      modifyListProperty  ,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER    ��  4�  @�      hideObject  ,   $�  T�  l�      editInstanceProperties  ,   D�  ��  ��      displayLinks    ,   p�  ��  ��      createControls  ,   ��  ��  ��      changeCursor    ,INPUT pcCursor CHARACTER   ��  �  �      applyEntry  ,INPUT pcField CHARACTER    ��  <�  L�      adjustTabOrder  ,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER ,�  ��  ��      addMessage  ,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER ��  �  �      addLink ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE ��  d�  t�      unbindServer    ,INPUT pcMode CHARACTER T�  ��  ��      startServerObject   ,   ��  ��  ��      runServerObject ,INPUT phAppService HANDLE  ��   �  �      restartServerObject ,   ��  (�  @�      initializeServerObject  ,   �  T�  h�      disconnectObject    ,   D�  |�  ��      destroyServerObject ,   l�  ��  ��      bindServer  ,   ��  ��  ��      processAction   ,INPUT pcAction CHARACTER   ��   �  �      enableObject    ,   ��  $�  4�      disableObject   ,   �  H�  T�      applyLayout ,   8�  h�  t�      viewPage    ,INPUT piPageNum INTEGER    X�  ��  ��      viewObject  ,   ��  ��  ��      toolbar ,INPUT pcValue CHARACTER    ��  ��   �      selectPage  ,INPUT piPageNum INTEGER    ��  ,�  @�      removePageNTarget   ,INPUT phTarget HANDLE,INPUT piPage INTEGER �  |�  ��      passThrough ,INPUT pcLinkName CHARACTER,INPUT pcArgument CHARACTER  l�  ��  ��      notifyPage  ,INPUT pcProc CHARACTER ��  �  �      initPages   ,INPUT pcPageList CHARACTER ��  <�  X�      initializeVisualContainer   ,   ,�  l�  ��      initializeObject    ,   \�  ��  ��      hidePage    ,INPUT piPageNum INTEGER    ��  ��  ��      destroyObject   ,   ��  ��  ��      deletePage  ,INPUT piPageNum INTEGER    ��  (�  8�      createObjects   ,   �  L�  \�      constructObject ,INPUT pcProcName CHARACTER,INPUT phParent HANDLE,INPUT pcPropList CHARACTER,OUTPUT phObject HANDLE <�  ��  ��      confirmExit ,INPUT-OUTPUT plCancel LOGICAL  ��  �  �      changePage  ,   ��  ,�  @�      assignPageProperty  ,INPUT pcProp CHARACTER,INPUT pcValue CHARACTER      � 
"     
 0%     adecomm/as-utils.w 
"   
   �    }        �
"     
       �     }        �G� �   �G%              � �     %        %       	 %        %       %        %       %               %               %               %              %              %              %               %              
�        
"   
 0
�    
"   
 0
"   
 �    �        �     �        �    
"   
   �        0         �     }        �%              
"   
 0
"   
 �    �        �     �        �    
"   
   �        �         �     }        �%              � 
"    
 �%              � �  �         X      $              
�    � 
   �     
"   
                       
�            �    �
"    
 �
�H T   %              �     }        �GG %              � 
"  
 
   P �L 
�H T   %              �     }        �GG %              
"  	 
   �        �    7%               
"  	 
  �           �    1�   
  � '   �%               o%   o           � ,     
"  	 
  �           0    1� -    � '   �%               o%   o           � ;    
"  	 
  �           �    1� B  
  � '   �%               o%   o           � M    
"  	 
  �               1� Y    � '   �%               o%   o           � g    
"  	 
  �           �    1� n    � '   �%               o%   o           � }    
"  	 
  �                1� �    � �   �%               o%   o           %               
"  	 
 ��          |    1� �   �� �     
"  	 
  �           �    1� �    � '   �%               o%   o           � �  e  
"  	 
  �           ,    1� 8    � '   �%               o%   o           � G  [  
"  	 
  �           �    1� �    � �   �%               o%   o           %               
"  	 
  �           	    1� �    � �   �%               o%   o           %               
"  	 
  �           �	    1� �    � �   �%               o%   o           %              
"  	 
 ��          
    1� �   �� �     
"  	 
  �           P
    1� �  
  � �   �%               o%   o           %               
"  	 
  �           �
    1� �    � '   �%               o%   o           � ,     
"  	 
 ��          @    1� �   �� �     
"  	 
  �           |    1�     � '   �%               o%   o           �   t  
"  	 
 ��          �    1� �  
 �� �     
"  	 
  �           ,    1� �    � '   �%               o%   o           � �  �  
"  	 
  �           �    1� 8    � '   �%               o%   o           � ,     
"  	 
  �               1� O  
  � Z   �%               o%   o           %               
"  	 
 �           �    1� ^   � �   �%               o%   o           %               
"  	 
 ]�               1� f   ]� '   �%               o%   o           � ,    
"  	 
 ]�           �    1� w   ]� '   �%               o%   o           o%   o           
"  	 
 �           �    1� �  
 � '   �%               o%   o           � ,    
"  	 
 ]�           p    1� �   ]� �  	 �%               o%   o           � �  / 
"  	 
 ��          �    1� �   �� �  	   
"  	 
 �                1� �   � �  	 �o%   o           o%   o           � ,    
"  	 
 ��          �    1�    �� �  	   
"  	 
 �           �    1�    � �  	 �o%   o           o%   o           � ,    
"  	 
 ��          D    1� !   �� �     
"  	 
 ��          �    1� /   �� �  	   
"  	 
 ��          �    1� <   �� �  	   
"  	 
 ��          �    1� I   �� �  	   
"  	 
 �           4    1� W   � �   �o%   o           o%   o           %              
"  	 
 ��          �    1� h   �� �  	   
"  	 
 ��          �    1� v  
 �� �     
"  	 
 ��          (    1� �   �� �  	   
"  	 
 ��          d    1� �   �� �  	   
"  	 
 ��          �    1� �   �� �  	   
"  	 
 ��          �    1� �   �� �  	   
"  	 
 ��              1� �  	 �� �  	   
"  	 
 ��          T    1� �   �� �  	   
"  	 
 ��          �    1� �   �� �  	   
"  	 
 ]�           �    1�    ]� '   �%               o%   o           o%   o           
�H T   %              �     }        �GG %              
"   
   
"   
 
"   
   
"   
 �(�  L ( l       �        �    ��    � P   �        �    �@    
� @  , 
�       �    ��      p�               �L
�    %              � 8      �    � $         �           
�    � 9     
"   
 �� @  , 
�       �    �� B  
 �p�               �L"      P �L 
�H T   %              �     }        �GG %              
"  	 
 �           t    1� <  
 � '   �%               o%   o           � ,    
"  	 
 �           �    1� G  
 � '   �%               o%   o           o%   o           
"  	 
 �           d    1� R   � �   �%               o%   o           o%   o           
"  	 
 ]�           �    1� [   ]� �   �%               o%   o           %               
"  	 
 �           \    1� j   � �   �%               o%   o           %               
"  	 
 0�           �    1� w   0� '   �%               o%   o           � ,    
"  	 
 �           L    1� ~   � �   �%               o%   o           %              
"  	 
 �           �    1� �   � �   �%               o%   o           o%   o           
"  	 
 �           D    1� �   � '   �%               o%   o           o%   o           
"  	 
 �           �    1� �  	 � '   �%               o%   o           � ,    
"  	 
 �           4    1� �   � '   �%               o%   o           o%   o           
"  	 
 �           �    1� �   � '   �%               o%   o           o%   o           
"  	 
 �           ,    1� �   � �   �%               o%   o           %               
"  	 
 �           �    1� �   � �   �%               o%   o           o%   o           P �L 
�H T   %              �     }        �GG %              
"  	 
 �           x    1� �   � �  	 �%               o%   o           � ,    
"  	 
 �           �    1�     � �  	 �%               o%   o           � ,    
"  	 
 �           `    1�    � �   �%               o%   o           %               
"  	 
 0�           �    1�    0� �  	 �%               o%   o           � ,    
"  	 
 �           P     1� +   � �  	 �%               o%   o           � ,    0
"  	 
 �           �     1� 9   � �   �%               o%   o           %               
"  	 
 ]�           @!    1� G   ]� �  	 �%               o%   o           � ,    
"  	 
 �           �!    1� V   � �  	 �%               o%   o           � ,    ]
"  	 
 �           ("    1� e   � �  	 �%               o%   o           � ,    
"  	 
 �           �"    1� s   � �  	 �%               o%   o           o%   o           
"  	 
 �           #    1� �   � �  	 �%               o%   o           � ,    
"  	 
 0�           �#    1� �   0� �  	 �%               o%   o           � ,    
"  	 
 �            $    1� �  	 � �   �%               o%   o           %               
"  	 
 �           |$    1� �   � �   �%               o%   o           %               
"  	 
 �           �$    1� �   � �   �%               o%   o           o%   o           
"  	 
 ]�           t%    1� �   ]� �   �%               o%   o           o%   o           
"  	 
 �           �%    1� �   � �   �%               o%   o           %               
"  	 
 �           l&    1� �   � �   �%               o%   o           %               
"  	 
 �           �&    1� �   � �   �%               o%   o           %               
"  	 
 0�           d'    1�    0�    �%               o%   o           %       
       
"  	 
 0�           �'    1�    0�    �%               o%   o           o%   o           
"  	 
 �           \(    1� &   �    �%               o%   o           %              
"  	 
 �           �(    1� 2   �    �%               o%   o           o%   o           
"  	 
 �           T)    1� >   �    �%               o%   o           %              
"  	 
 �           �)    1� K   �    �%               o%   o           o%   o           
"  	 
 �           L*    1� X   �    �%               o%   o           %              
"  	 
 �           �*    1� `   �    �%               o%   o           o%   o           
"  	 
 0�           D+    1� h   0� �  	 �%               o%   o           � ,    P �L 
�H T   %              �     }        �GG %              
"  	 
 �           ,    1� z   � Z   �%               o%   o           %               
"  	 
 �           �,    1� �   � Z   �%               o%   o           o%   o           
"  	 
 �           -    1� �   � '   �%               o%   o           � ,    
"  	 
 �           x-    1� �   � '   �%               o%   o           � �  - 
"  	 
 �           �-    1� �   � '   �%               o%   o           � ,    
"  	 
 �           `.    1� �   � '   �%               o%   o           �    
"  	 
 ��          �.    1� 8   �� �     
"  	 
 �           /    1� I   � '   �%               o%   o           � ,    
"  	 
 ��          �/    1� U  
 �� �     
"  	 
 ��          �/    1� `   �� �     
"  	 
 �           �/    1� m   � �  	 �%               o%   o           � ,    
"  	 
 �           p0    1� z   � '   �%               o%   o           � ,    
"  	 
 �           �0    1� �   � �   �%               o%   o           o%   o           
"  	 
 �           `1    1� �   � '   �%               o%   o           � �  ! ]
"  	 
 �           �1    1� �   � '   �%               o%   o           � ,    
"  	 
 0�           H2    1� �   0� '   �%               o%   o           � �   
"  	 
 0�           �2    1� �  	 0� Z   �%               o%   o           o%   o           
"  	 
 �           83    1�    � �   �%               o%   o           %               
"  	 
 ��          �3    1�    �� �     
"  	 
 �           �3    1�    � '   �%               o%   o           � 0   
"  	 
 ]�           d4    1� ?   ]� �  	 �%               o%   o           � ,    
"  	 
 �           �4    1� L   � �  	 �%               o%   o           � ,    ]
"  	 
 ��          L5    1� \   �� �     
"  	 
 ��          �5    1� n   �� �  	   
"  	 
 0�           �5    1� �   0� �   �o%   o           o%   o           %               
"  	 
 ��          @6    1� �   �� �     
"  	 
 ��          |6    1� �   �� �  	   
"  	 
 ��          �6    1� �   �� �  	   
"  	 
 ��          �6    1� �   �� �  	   
"  	 
 ��          07    1� �   �� �  	   
"  	 
 ��          l7    1� �   �� �  	   
"  	 
 ��          �7    1�    �� �     
"  	 
 �           �7    1�    � '   �%               o%   o           � +  4 
"  	 
 ��          X8    1� `   �� �     
"  	 
 ��          �8    1� m   �� �     
"  	 
 ��          �8    1� }   �� �     
"  	 
 ��          9    1� �   �� �  	   
"  	 
 ��          H9    1� �   �� �  	   
"  	 
 ��          �9    1� �   �� �  	   
"  	 
 ��          �9    1� �   �� �     
"  	 
 �           �9    1� �   � �  	 �%               o%   o           � ,    
"  	 
 �           p:    1� �   � �  	 �%               o%   o           � ,    
"  	 
 �           �:    1� �   � �  	 �%               o%   o           � ,    
"  	 
 �           X;    1� �   � �  	 �%               o%   o           � ,    
"  	 
 �           �;    1�    � �   �%               o%   o           %               
"  	 
 �           H<    1� !   � �   �%               o%   o           o%   o           
"  	 
 ]�           �<    1� 3   ]� �   �%               o%   o           %               
"  	 
 �           @=    1� C   � �   �%               o%   o           %               
"  	 
 �           �=    1� O   � �   �%               o%   o           o%   o           
"  	 
 �           8>    1� j   � �   �%               o%   o           %               
"  	 
 ��          �>    1� x   �� �  	   
"  	 
 �           �>    1� �   � �   �%               o%   o           %              
"  	 
 ��          l?    1� �   �� �  	   
"  	 
 ��          �?    1� �   �� �  	   
"  	 
 ��          �?    1� �  
 �� �  	   
"  	 
 �            @    1� �   � �  	 �%               o%   o           �    
"  	 
 �           �@    1� �   � �  	 �%               o%   o           � ,    
"   
    "    �%     start-super-proc ��%     adm2/smart.p \�P �L 
�H T   %              �     }        �GG %              
"  	 
   �       �A    6�      
"  	 
   
�        �A    8
"  
 
   �         B    ��     }        �G 4              
"  
 
 ߱G %              G %              %p e `   LogicalObjectName,PhysicalObjectName,DynamicObject,RunAttribute,HideOnInit,DisableOnInit,ObjectLayout �
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        HC    ��    � P   �        TC    �@    
� @  , 
�       `C    ��    �p�               �L
�    %              � 8      lC    � $         �           
�    � 9   �
"   
 �p� @  , 
�       |D    �� �   �p�               �L"    , �   �    �    ��     }        �A      |    "      �    %              (<   \ (    |    �     }        �A�    �A"        "    �"      < "    �"    (    |    �     }        �A�    �A"    
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        PF    ��    � P   �        \F    �@    
� @  , 
�       hF    ��    �p�               �L
�    %              � 8      tF    � $         �           
�    � 9   �
"   
 �p� @  , 
�       �G    ��   
 �p�               �L"    , 
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        (H    ��    � P   �        4H    �@    
� @  , 
�       @H    ��    �p�               �L
�    %              � 8      LH    � $         �           
�    � 9   �
"   
 �p� @  , 
�       \I    �� �   �p�               �L
"   
 , 
�H T   %              �     }        �GG %              
"   
   
"   
 
"   
   
"   
   (�  L ( l       �         J    ��    � P   �        J    �@    
� @  , 
�       J    ��      p�               �L
�    %              � 8      $J    � $         �           
�    � 9     
"   
 �p� @  , 
�       4K    �� B  
 �p�               �L%     SmartWindow 
"   
   p� @  , 
�       �K    �� Y     p�               �L%      WINDOW  
"   
  p� @  , 
�       �K    ��     p�               �L%               
"   
  p� @  , 
�       XL    �� �    p�               �L(        � ,      � ,      � ,      �     }        �A
�H T   %              �     }        �GG %              
"   
  (   � 
"   
 �    �        8M    ��    �
"   
   � 8      �M    � $         �           
�    � 9   �
"   
   �        �M    �
"   
   �       �M    /
"   
   
"   
   �       (N    6�      
"   
   
�        TN    8
"   
   �        tN    �
"   
   �       �N    �
"   
   p�    � 9   
�    �     }        �G 4              
"   
 ߱G %              G %              
�     }        �
"   
    (   � 
"   
 �    �        XO    �A"    �A
"   
   
�        �O    �@ � 
"   
 "      �       }        �
"   
 �%              %                "    �%     start-super-proc ��%     adm2/appserver.p �    � �     
�    �     }        �%               %      Server  - �     }        �    "    � ,    �%                   "    � ,    �%      NONE    p�,  8         $     "    ]        � �   �
�    
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        �Q    ��    � P   �        �Q    �@    
� @  , 
�       �Q    ��    �p�               �L
�    %              � 8      R    � $         �           
�    � 9   �
"   
 �p� @  , 
�       S    �� �   �p�               �L"    , p�,  8         $     "    ]        � �   �
�     "    �%     start-super-proc ��%     adm2/visual.p �� 
"    
 �%     contextHelp 
"    
   
�    
�    %     processAction   
�    %     CTRL-PAGE-UP \�%     processAction   
�    %     CTRL-PAGE-DOWN  "    �%     start-super-proc �%     adm2/containr.p %     modifyListProperty 
�    
�    %      Add     %      ContainerSourceEvents %      initializeDataObjects 0 0   A    �    � L   
�    � ^   �A    �    � L     
�    � j   �%     modifyListProperty 
�    
�    %      Add     %      ContainerSourceEvents %     buildDataRequest ent0 A    �    � L   �
�    � �   %     modifyListProperty 
�    
�    %      Add     %     SupportedLinks %      ContainerToolbar-Target � 
"    
 �
"   
 �%     contextHelp 
"    
   
�    
�    %               
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
 (�  L ( l       �        �W    ��    � P   �        �W    �@    
� @  , 
�       X    ��    �p�               �L
�    %              � 8      X    � $         �    �     
�    � 9   �
"   
 �p� @  , 
�       $Y    �� \   �p�               �L
�             �G
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
 �(�  L ( l       �        �Y    ��    � P   �        �Y    �@    
� @  , 
�       �Y    ��    �p�               �L
�    %              � 8      �Y    � $         �    �     
�    � 9   �
"   
 �p� @  , 
�       [    ��    �p�               �L%              (        �     }        �G� �   �G� 
"   
 �
"   
   �        �[    �%              
"   
 �
"   
 ��     }        �%               
"   
 �%      CLOSE   %               � 
"   
 �
"   
 
"   
 ��        h\    %%              
�     }        �
"   
   %     destroyObject       �     }        �    �  � �  	   %               
"   
 �
�    %     createObjects    �     }        �%     initializeObject � �     }        ��     "      %               %     constructObject  ,         "    t G %              � �   �
�             �G%LA<  AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedalmacenOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes �
"   
   %     repositionObject �
"   
   %         %           %     constructObject "      
�             �G%� � �   EnabledObjFldsToDisable?ModifyFields(All)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout 
"   
   %     repositionObject �
"   
   %       	  %            %     constructObject %     adm2/pupdsav.w 
�             �G%$  AddFunctionOne-RecordPanelTypeUpdateAddFunctionOne-RecordEdgePixels2DeactivateTargetOnHidenoDisabledActionsFlatButtonsyesMenuyesShowBorderyesToolbaryesActionGroupsTableio,NavigationTableIOTypeUpdateSupportedLinksTableIO-SourceToolbarBandsToolbarAutoSizenoToolbarDrawDirectionhorizontalLogicalObjectNameDisabledActionsHiddenActionsHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0RemoveMenuOnHidenoCreateSubMenuOnConflictyesNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout \�
"   
   %     repositionObject �
"   
   %         %            %     resizeObject    
"   
   %         %        %      addLink 
"   
   %      Data    
"   
   %      addLink 
"   
   %      Update  
"   
   %      addLink 
"   
   %      TableIO 
"   
   %     adjustTabOrder  
"   
   
"   
   %      AFTER   (        �     }        �G� �   �G� 
"   
 �
"   
   �     }        �
�    
"   
   
"   
   %      CLOSE   %                               �           �   l       ��                   7  �               �m                    O   ����    e�          O   ����    R�          O   ����    ��        $  "  �   ���                       �L     
                    � ߱              #  (  �      DM      4   ����DM                �                      ��                  $  6                  �t                       $  8  �  �  %  �M            '  �  `      �M      4   �����M                p                      ��                  (  5                  �v                       (  �  �  o   )      ,                                 �  �   *  N      �  �   +  4N      $  $  ,  �  ���                       `N     
                    � ߱        8  �   -  �N      L  �   .  �N      `  �   1  �N          $   4  �  ���                       �N  @         �N              � ߱                     T          ,  @   T �            
             
             
             
                 $   4   D          $   4   D   ����        ��                            ����                                            �           �   l       ��                 [  �  �               �w                    O   ����    e�          O   ����    R�          O   ����    ��      Z                      �          �  $  m    ���                       DO     
                    � ߱                  �  �                      ��                   n  p                  �}                     n  4      4   ����dO      $  o  �  ���                       �O     
                    � ߱        �    q  4  D      �O      4   �����O      /  r  p                               3   �����O  �  �   �  �O          O   �  ��  ��  P                               , �                          
                               �      ��                            ����                                            �           �   l       ��                 �    �               ��                    O   ����    e�          O   ����    R�          O   ����    ��      �]                         � ߱          $  �  �   ���                           p   �  �]  (             �     �]                �                      ��                  �  �                   �                       �  8    /   �  �     �                          3   �����]                                 3   �����]  P     
   @                      3   ����$^  �        p                      3   ����8^         
   �  �                  3   �����_      $   �  �  ���                               
                    � ߱        �  /	  �  4     D  �_                      3   �����_  t        d                      3   �����_            �                      3   �����_  �  /   �  �     �                          3   �����_                                 3   ����`  @     
   0                      3   ����`  p        `                      3   ����$`         
   �  �                  3   ����a      $   �  �  ���                               
                    � ߱        �  /	  �  $     4  4a                      3   ����a  d        T                      3   ����@a            �                      3   ����Ta  �  /   �  �     �                          3   ����ha           �                      3   �����a  0     
                          3   �����a  `        P                      3   �����a         
   �  �                  3   �����c      $   �  �  ���                               
                    � ߱        �  /	  �       $  d                      3   �����c  T        D                      3   ����d            t                      3   ����(d     /	  �  �     �  Xd                      3   ����<d  �        �                      3   ����dd                                  3   ����xd  �  /   �  L     \                          3   �����d  �     
   |                      3   �����d  �        �                      3   �����d         
   �                      3   �����d  �	  /   �  	     (	                          3   �����d  X	     
   H	                      3   �����d  �	        x	                      3   �����d         
   �	                      3   ���� e  �
  /   �  �	     �	                          3   ����e  $
     
   
                      3   ���� e  T
        D
                      3   ����,e         
   t
                      3   ����@e      /   �  �
     �
                          3   ����Le  �
     
   �
                      3   ����he        
                         3   ����te            @                      3   �����e               �          �  �    �                                             ��                              ��        �                  ����                                            �           �   l       ��                      �               d�                    O   ����    e�          O   ����    R�          O   ����    ��             �   �       �e      4   �����e      n        �          �e            ,      �e      4   �����e      �     �e    ��                            ����                                            �           �   l       ��                    (  �               ȓ                    O   ����    e�          O   ����    R�          O   ����    ��      �   
   %  �� �   �e                
   '  �� �              f    ��                              ��        �                  ����                                            �           �   l       ��                  .  8  �               X�                    O   ����    e�          O   ����    R�          O   ����    ��      �     5  f  }          O   6  ��  ��  (f    ��                            ����                                   d d     ,    ���#��#  � �                                               �                                                                         d     D                                                                  D                                                                    TXS appSrvUtils ASSIGNFOCUSEDWIDGET ASSIGNWIDGETVALUE ASSIGNWIDGETVALUELIST BLANKWIDGET CLEARWIDGET DISABLERADIOBUTTON DISABLEWIDGET ENABLERADIOBUTTON ENABLEWIDGET FORMATTEDWIDGETVALUE FORMATTEDWIDGETVALUELIST HIDEWIDGET HIGHLIGHTWIDGET RESETWIDGETVALUE TOGGLEWIDGET VIEWWIDGET WIDGETHANDLE WIDGETLONGCHARVALUE WIDGETISBLANK WIDGETISFOCUSED WIDGETISMODIFIED WIDGETISTRUE WIDGETVALUE WIDGETVALUELIST cSDOName cSDVName wWin h_dalmacen h_pupdsav h_valmacen fMain GUI <insert SmartWindow title> DISABLEPAGESINFOLDER ENABLEPAGESINFOLDER GETCALLERPROCEDURE GETCALLERWINDOW GETCONTAINERMODE GETCONTAINERTARGET GETCONTAINERTARGETEVENTS GETCURRENTPAGE GETDISABLEDADDMODETABS GETDYNAMICSDOPROCEDURE GETFILTERSOURCE GETMULTIINSTANCEACTIVATED GETMULTIINSTANCESUPPORTED GETNAVIGATIONSOURCE GETNAVIGATIONSOURCEEVENTS GETNAVIGATIONTARGET GETOUTMESSAGETARGET GETPAGENTARGET GETPAGESOURCE GETPRIMARYSDOTARGET GETREENABLEDATALINKS GETRUNDOOPTIONS GETRUNMULTIPLE GETSAVEDCONTAINERMODE GETSDOFOREIGNFIELDS GETTOPONLY GETUPDATESOURCE GETUPDATETARGET GETWAITFOROBJECT GETWINDOWTITLEVIEWER GETSTATUSAREA PAGENTARGETS SETCALLEROBJECT SETCALLERPROCEDURE SETCALLERWINDOW SETCONTAINERMODE SETCONTAINERTARGET SETCURRENTPAGE SETDISABLEDADDMODETABS SETDYNAMICSDOPROCEDURE SETFILTERSOURCE SETINMESSAGETARGET SETMULTIINSTANCEACTIVATED SETMULTIINSTANCESUPPORTED SETNAVIGATIONSOURCE SETNAVIGATIONSOURCEEVENTS SETNAVIGATIONTARGET SETOUTMESSAGETARGET SETPAGENTARGET SETPAGESOURCE SETPRIMARYSDOTARGET SETREENABLEDATALINKS SETROUTERTARGET SETRUNDOOPTIONS SETRUNMULTIPLE SETSAVEDCONTAINERMODE SETSDOFOREIGNFIELDS SETTOPONLY SETUPDATESOURCE SETUPDATETARGET SETWAITFOROBJECT SETWINDOWTITLEVIEWER GETOBJECTTYPE SETSTATUSAREA GETALLFIELDHANDLES GETALLFIELDNAMES GETCOL GETDEFAULTLAYOUT GETDISABLEONINIT GETENABLEDOBJFLDS GETENABLEDOBJHDLS GETHEIGHT GETHIDEONINIT GETLAYOUTOPTIONS GETLAYOUTVARIABLE GETOBJECTENABLED GETOBJECTLAYOUT GETROW GETWIDTH GETRESIZEHORIZONTAL GETRESIZEVERTICAL SETALLFIELDHANDLES SETALLFIELDNAMES SETDEFAULTLAYOUT SETDISABLEONINIT SETHIDEONINIT SETLAYOUTOPTIONS SETOBJECTLAYOUT SETRESIZEHORIZONTAL SETRESIZEVERTICAL GETOBJECTTRANSLATED GETOBJECTSECURED CREATEUIEVENTS GETAPPSERVICE GETASBOUND GETASDIVISION GETASHANDLE GETASHASSTARTED GETASINFO GETASINITIALIZEONRUN GETASUSEPROMPT GETSERVERFILENAME GETSERVEROPERATINGMODE RUNSERVERPROCEDURE SETAPPSERVICE SETASDIVISION SETASHANDLE SETASINFO SETASINITIALIZEONRUN SETASUSEPROMPT SETSERVERFILENAME SETSERVEROPERATINGMODE gshAstraAppserver gshSessionManager gshRIManager gshSecurityManager gshProfileManager gshRepositoryManager gshTranslationManager gshWebManager gscSessionId gsdSessionObj gshFinManager gshGenManager gshAgnManager gsdTempUniqueID gsdUserObj gsdRenderTypeObj gsdSessionScopeObj ghProp ghADMProps ghADMPropsBuf glADMLoadFromRepos glADMOk ANYMESSAGE ASSIGNLINKPROPERTY FETCHMESSAGES GETCHILDDATAKEY GETCONTAINERHANDLE GETCONTAINERHIDDEN GETCONTAINERSOURCE GETCONTAINERSOURCEEVENTS GETCONTAINERTYPE GETDATALINKSENABLED GETDATASOURCE GETDATASOURCEEVENTS GETDATASOURCENAMES GETDATATARGET GETDATATARGETEVENTS GETDBAWARE GETDESIGNDATAOBJECT GETDYNAMICOBJECT GETINSTANCEPROPERTIES GETLOGICALOBJECTNAME GETLOGICALVERSION GETOBJECTHIDDEN GETOBJECTINITIALIZED GETOBJECTNAME GETOBJECTPAGE GETOBJECTPARENT GETOBJECTVERSION GETOBJECTVERSIONNUMBER GETPARENTDATAKEY GETPASSTHROUGHLINKS GETPHYSICALOBJECTNAME GETPHYSICALVERSION GETPROPERTYDIALOG GETQUERYOBJECT GETRUNATTRIBUTE GETSUPPORTEDLINKS GETTRANSLATABLEPROPERTIES GETUIBMODE GETUSERPROPERTY INSTANCEPROPERTYLIST LINKHANDLES LINKPROPERTY MAPPEDENTRY MESSAGENUMBER PROPERTYTYPE REVIEWMESSAGES SETCHILDDATAKEY SETCONTAINERHIDDEN SETCONTAINERSOURCE SETCONTAINERSOURCEEVENTS SETDATALINKSENABLED SETDATASOURCE SETDATASOURCEEVENTS SETDATASOURCENAMES SETDATATARGET SETDATATARGETEVENTS SETDBAWARE SETDESIGNDATAOBJECT SETDYNAMICOBJECT SETINSTANCEPROPERTIES SETLOGICALOBJECTNAME SETLOGICALVERSION SETOBJECTNAME SETOBJECTPARENT SETOBJECTVERSION SETPARENTDATAKEY SETPASSTHROUGHLINKS SETPHYSICALOBJECTNAME SETPHYSICALVERSION SETRUNATTRIBUTE SETSUPPORTEDLINKS SETTRANSLATABLEPROPERTIES SETUIBMODE SETUSERPROPERTY SHOWMESSAGE SIGNATURE , prepareInstance ObjectName CHAR  ObjectVersion ADM2.2 ObjectType SmartWindow ContainerType WINDOW PropertyDialog adm2/support/visuald.w QueryObject LOGICAL ContainerHandle HANDLE InstanceProperties LogicalObjectName,PhysicalObjectName,DynamicObject,RunAttribute,HideOnInit,DisableOnInit,ObjectLayout SupportedLinks Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source ContainerHidden ObjectInitialized ObjectHidden ObjectsCreated HideOnInit UIBMode ContainerSource ContainerSourceEvents initializeObject,hideObject,viewObject,destroyObject,enableObject,confirmExit,confirmCancel,confirmOk,isUpdateActive DataSource DataSourceEvents dataAvailable,queryPosition,updateState,deleteComplete,fetchDataSet,confirmContinue,confirmCommit,confirmUndo,assignMaxGuess,isUpdatePending TranslatableProperties ObjectPage INT DBAware DesignDataObject DataSourceNames DataTarget DataTargetEvents CHARACTER updateState,rowObjectState,fetchBatch,LinkState LogicalObjectName PhysicalObjectName LogicalVersion PhysicalVersion DynamicObject RunAttribute ChildDataKey ParentDataKey DataLinksEnabled InactiveLinks InstanceId DECIMAL SuperProcedure SuperProcedureMode SuperProcedureHandle LayoutPosition ClassName RenderingProcedure ThinRenderingProcedure Label cType ADMProps Target WHERE Target = WIDGET-H(" ") AppService ASDivision ASHandle ASHasConnected ASHasStarted ASInfo ASInitializeOnRun ASUsePrompt BindSignature BindScope ServerOperatingMode ServerFileName ServerFirstCall NeedContext ObjectLayout LayoutOptions ObjectEnabled LayoutVariable DefaultLayout DisableOnInit EnabledObjFlds EnabledObjHdls FieldSecurity SecuredTokens AllFieldHandles AllFieldNames MinHeight MinWidth ResizeHorizontal ResizeVertical ObjectSecured ObjectTranslated PopupButtonsInFields ColorInfoBG INTEGER ColorInfoFG ColorWarnBG ColorWarnFG ColorErrorBG ColorErrorFG BGColor FGColor FieldPopupMapping CurrentPage PendingPage ContainerTarget ContainerTargetEvents exitObject,okObject,cancelObject,updateActive ContainerToolbarSource ContainerToolbarSourceEvents toolbar,okObject,cancelObject OutMessageTarget PageNTarget PageSource FilterSource UpdateSource UpdateTarget CommitSource CommitSourceEvents commitTransaction,undoTransaction CommitTarget CommitTargetEvents rowObjectState StartPage RunMultiple WaitForObject DynamicSDOProcedure adm2/dyndata.w RunDOOptions InitialPageList WindowFrameHandle Page0LayoutManager MultiInstanceSupported MultiInstanceActivated ContainerMode SavedContainerMode SdoForeignFields NavigationSource NavigationTarget PrimarySdoTarget NavigationSourceEvents fetchFirst,fetchNext,fetchPrev,fetchLast,startFilter CallerWindow CallerProcedure CallerObject DisabledAddModeTabs ReEnableDataLinks WindowTitleViewer UpdateActive InstanceNames ClientNames ContainedDataObjects ContainedAppServices DataContainer HasDbAwareObjects HasDynamicProxy HideOnClose HideChildContainersOnClose HasObjectMenu RequiredPages RemoveMenuOnHide ProcessList PageLayoutInfo PageTokens DataContainerName WidgetIDFileName ghContainer adm2/smart.p cObjectName iStart / \ . hReposBuffer hPropTable hBuffer hTable deleteProperties ADM-CLONE-PROPS pcProcName hProc START-SUPER-PROC cAppService cASDivision cServerOperatingMode adm2/appserver.p getAppService Server NONE setASDivision setAppService cFields adm2/visual.p CTRL-PAGE-UP CTRL-PAGE-DOWN adm2/containr.p Add initializeDataObjects getSupportedLinks data-target data-source buildDataRequest containertoolbar-target ContainerToolbar-Target CLOSE iStartPage ADM-ERROR currentPage DB-AWARE AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedalmacenOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes EnabledObjFldsToDisable?ModifyFields(All)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout adm2/pupdsav.w AddFunctionOne-RecordPanelTypeUpdateAddFunctionOne-RecordEdgePixels2DeactivateTargetOnHidenoDisabledActionsFlatButtonsyesMenuyesShowBorderyesToolbaryesActionGroupsTableio,NavigationTableIOTypeUpdateSupportedLinksTableIO-SourceToolbarBandsToolbarAutoSizenoToolbarDrawDirectionhorizontalLogicalObjectNameDisabledActionsHiddenActionsHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0RemoveMenuOnHidenoCreateSubMenuOnConflictyesNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout Data Update TableIO AFTER ADM-CREATE-OBJECTS DISABLE_UI ENABLE_UI EXITOBJECT �  t      �#      % �8   ��      0         pcProp      ��      P         pcProp      ��      p         plCancel    �   ��      �         pcProcName  �   ��      �        
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
 hTarget X  ��      L        pcMessage       ��      p        pcMessage       ��      �        plEnabled             �     cType       �     6   �          �                  getObjectType   	  /	  1	  ,          
   hReposBuffer    L        @  
   hPropTable  h        `  
   hBuffer           |  
   hTable  �  �     7             �                  adm-clone-props "  #  $  %  '  (  )  *  +  ,  -  .  1  4  5  6  7              
   hProc             <        pcProcName  �  �  	   8     $      x                  start-super-proc    m  n  o  p  q  r  �  �  �  H  �     9                                   F  �  	     :                                   J  K  �  L	     ;                                   N  O  	  �	     <                                   �  T	  �	     =                                   i  j  �	  �	     >                                   u  v  w  �	  ,
     ?                                   �  �  �  �  �	  l
     @                                   �  �            �
     currentPage <
  �
     A   t
          �
                  adm-create-objects  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �       �
  X     B               L                  disable_UI            �     C               �                  enable_UI   %  '  (  h  �     D               �                  exitObject  5  6  8  �  4       (                              H          <  
   appSrvUtils h       \     cSDOName    �       |     cSDVName    �       �  
   wWin    �       �  
   h_dalmacen  �       �  
   h_pupdsav          �  
   h_valmacen  ,          
   gshAstraAppserver   T        @  
   gshSessionManager   x        h  
   gshRIManager    �        �  
   gshSecurityManager  �        �  
   gshProfileManager   �        �  
   gshRepositoryManager       	 	       
   gshTranslationManager   D  
 
     4  
   gshWebManager   h        X     gscSessionId    �        |     gsdSessionObj   �        �  
   gshFinManager   �        �  
   gshGenManager   �        �  
   gshAgnManager                gsdTempUniqueID <        0     gsdUserObj  d        P     gsdRenderTypeObj    �        x     gsdSessionScopeObj  �       �  
   ghProp  �    	   �  
   ghADMProps  �    
   �  
   ghADMPropsBuf                glADMLoadFromRepos  0       (     glADMOk P       D  
   ghContainer p       d     cObjectName �       �     iStart  �       �     cAppService �       �     cASDivision �       �     cServerOperatingMode                cFields          (     iStartPage           7       0  _  `  b  c  f  g  i  �  �  �  �  �                               "  $  &  '  (  +  -  .  0  1  2  3  4  :  <  B  D  F  G  M  N  O  P  S  T  V  W  Y  Z  [  \  ]  ^  _  a  b  c  e  f  g  h  i  �  Q	  R	  U	  V	  W	  X	  Y	  Z	  [	  \	  ]	  ^	  _	  `	  a	  b	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	   
  
  r
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
  0  <  u  v    �  �  �  �  �  �  �  �  �  �        �  �  �  �  �  �  �  �  �  �  �  �    &  Y  �  �  C  D  E  G  I  M  f  g  h  j  r  x  ~  �  �  �  �  �  �  �     '  V  W  d  q  �  �  �  �  �  �  �  �  �  �  �  �  �      H� $ C:\Progress\OpenEdge\src\adm2\windowmn.i 8  f!  C:\Progress\OpenEdge\src\adm2\containr.i l  � # %C:\Progress\OpenEdge\src\adm2\custom\containrcustom.i    �  ��  C:\Progress\OpenEdge\src\adm2\visual.i   �  # " %C:\Progress\OpenEdge\src\adm2\custom\visualcustom.i    �<  C:\Progress\OpenEdge\src\adm2\appserver.i    X  �� ! %C:\Progress\OpenEdge\src\adm2\custom\appservercustom.i   �  I�  C:\Progress\OpenEdge\src\adm2\smart.i    �  Ds   C:\Progress\OpenEdge\gui\fn    tw  %C:\Progress\OpenEdge\src\adm2\custom\smartcustom.i   0  Q.  C:\Progress\OpenEdge\gui\set p  ��  C:\Progress\OpenEdge\src\adm2\cntnprop.i �  ��  %C:\Progress\OpenEdge\src\adm2\custom\cntnpropcustom.i    �  P  %C:\Progress\OpenEdge\src\adm2\custom\cntnprtocustom.i      F>  C:\Progress\OpenEdge\src\adm2\visprop.i  T  �I  %C:\Progress\OpenEdge\src\adm2\custom\vispropcustom.i �  ��  %C:\Progress\OpenEdge\src\adm2\custom\visprtocustom.i �  �l 
 C:\Progress\OpenEdge\src\adm2\appsprop.i   ɏ  %C:\Progress\OpenEdge\src\adm2\custom\appspropcustom.i    <  V  %C:\Progress\OpenEdge\src\adm2\custom\appsprtocustom.i    �  i$  C:\Progress\OpenEdge\src\adm2\smrtprop.i �  �j  C:\Progress\OpenEdge\gui\get �  �  %C:\Progress\OpenEdge\src\adm2\custom\smrtpropcustom.i       ��  %C:\Progress\OpenEdge\src\adm2\custom\smrtprtocustom.i    d  ��  C:\Progress\OpenEdge\src\adm2\smrtprto.i �  Su  C:\Progress\OpenEdge\src\adm2\globals.i  �  M�  %C:\Progress\OpenEdge\src\adm2\custom\globalscustom.i   )a  %C:\Progress\OpenEdge\src\adm2\custom\smartdefscustom.i   P  �  C:\Progress\OpenEdge\src\adm2\appsprto.i �  ��  %C:\Progress\OpenEdge\src\adm2\custom\appserverdefscustom.i   �  �X 	 C:\Progress\OpenEdge\src\adm2\visprto.i    !�  %C:\Progress\OpenEdge\src\adm2\custom\visualdefscustom.i  D  n�  C:\Progress\OpenEdge\src\adm2\cntnprto.i �  ;  %C:\Progress\OpenEdge\src\adm2\custom\containrdefscustom.i    �  ~�  C:\Progress\OpenEdge\src\adm2\widgetprto.i     e�  !C:\Progress\OpenEdge\gui\adecomm\appserv.i   <  ��   d:\newsie\on_in_co\APLIC\alm\walmacen.w      �   �      �     �  $   �  �   H      �  �   A     �          �  �        �     �       �   �          �  #   (  �   �     8           H  �   x     X     v      h  �   u     x     s      �  r   W     �  n   ?     �     �  "   �  P   �     �  �   �     �     h  !   �  �   c     �     A       �   @               (  �        8     �     H  g   �     X     �     h  O   �     x  �   3     �     1      �  �        �     �     �  �   �     �     |     �  �   {     �     Y     �  �   X           6        �   5     (           8   �        H      �     X   �   �     h      �     x   }   �     �      �     �           �      �     �      t     �   7   9     �   �   0     �   O   "     �           !     �
     !  �   {
     (!  �   r
     8!  O   d
     H!     S
     X!     
     h!  �   �	     x!  x   �	  
   �!  M   �	     �!     �	     �!     f	     �!  a   O	  
   �!  �  .	     �!     	     �!  �  �     �!  O   �     "     �     "     o     ("  �   �     8"     k     H"     �     X"  x   �     h"     �     x"     *     �"     &     �"          �"     �     �"  Q   �  
   �"     �     �"     W  
   �"     C     �"     )  
   #  f   �     #     �  	   (#  "   Y     8#     E     H#     $     X#  Z   �     h#     �     x#     �     �#     �     �#     n     �#     8     �#  '   �       �#     @      �#            �#           