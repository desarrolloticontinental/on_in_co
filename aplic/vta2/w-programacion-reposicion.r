	��V�:�ad5  ��              S                                �Q 35640110utf-8 MAIN d:\newsie\on_in_co\APLIC\vta2\w-programacion-reposicion.w,, PROCEDURE ue-verifica-stock,, PROCEDURE ue-procesar,, PROCEDURE ue-grabar,, PROCEDURE ue-gen-excel,, PROCEDURE ue-fecha-proceso,, PROCEDURE ue-cargar-articulos,, PROCEDURE exitObject,, PROCEDURE enable_UI,, PROCEDURE disable_UI,, PROCEDURE adm-create-objects,, PROCEDURE start-super-proc,,INPUT pcProcName CHARACTER PROCEDURE adm-clone-props,, PROCEDURE toggleData,,INPUT plEnabled LOGICAL PROCEDURE showMessageProcedure,,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL PROCEDURE returnFocus,,INPUT hTarget HANDLE PROCEDURE repositionObject,,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL PROCEDURE removeLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE removeAllLinks,, PROCEDURE modifyUserLinks,,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE PROCEDURE modifyListProperty,,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER PROCEDURE hideObject,, PROCEDURE editInstanceProperties,, PROCEDURE displayLinks,, PROCEDURE createControls,, PROCEDURE changeCursor,,INPUT pcCursor CHARACTER PROCEDURE applyEntry,,INPUT pcField CHARACTER PROCEDURE adjustTabOrder,,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER PROCEDURE addMessage,,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER PROCEDURE addLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE unbindServer,,INPUT pcMode CHARACTER PROCEDURE startServerObject,, PROCEDURE runServerObject,,INPUT phAppService HANDLE PROCEDURE restartServerObject,, PROCEDURE initializeServerObject,, PROCEDURE disconnectObject,, PROCEDURE destroyServerObject,, PROCEDURE bindServer,, PROCEDURE processAction,,INPUT pcAction CHARACTER PROCEDURE enableObject,, PROCEDURE disableObject,, PROCEDURE applyLayout,, PROCEDURE viewPage,,INPUT piPageNum INTEGER PROCEDURE viewObject,, PROCEDURE toolbar,,INPUT pcValue CHARACTER PROCEDURE selectPage,,INPUT piPageNum INTEGER PROCEDURE removePageNTarget,,INPUT phTarget HANDLE,INPUT piPage INTEGER PROCEDURE passThrough,,INPUT pcLinkName CHARACTER,INPUT pcArgument CHARACTER PROCEDURE notifyPage,,INPUT pcProc CHARACTER PROCEDURE initPages,,INPUT pcPageList CHARACTER PROCEDURE initializeVisualContainer,, PROCEDURE initializeObject,, PROCEDURE hidePage,,INPUT piPageNum INTEGER PROCEDURE destroyObject,, PROCEDURE deletePage,,INPUT piPageNum INTEGER PROCEDURE createObjects,, PROCEDURE constructObject,,INPUT pcProcName CHARACTER,INPUT phParent HANDLE,INPUT pcPropList CHARACTER,OUTPUT phObject HANDLE PROCEDURE confirmExit,,INPUT-OUTPUT plCancel LOGICAL PROCEDURE changePage,, PROCEDURE assignPageProperty,,INPUT pcProp CHARACTER,INPUT pcValue CHARACTER FUNCTION Signature,CHARACTER,INPUT pcName CHARACTER FUNCTION showmessage,LOGICAL,INPUT pcMessage CHARACTER FUNCTION setUserProperty,LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION setUIBMode,LOGICAL,INPUT pcMode CHARACTER FUNCTION setTranslatableProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setSupportedLinks,LOGICAL,INPUT pcLinkList CHARACTER FUNCTION setRunAttribute,LOGICAL,INPUT cRunAttribute CHARACTER FUNCTION setPhysicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setPhysicalObjectName,LOGICAL,INPUT cTemp CHARACTER FUNCTION setPassThroughLinks,LOGICAL,INPUT pcLinks CHARACTER FUNCTION setParentDataKey,LOGICAL,INPUT cParentDataKey CHARACTER FUNCTION setObjectVersion,LOGICAL,INPUT cObjectVersion CHARACTER FUNCTION setObjectParent,LOGICAL,INPUT phParent HANDLE FUNCTION setObjectName,LOGICAL,INPUT pcName CHARACTER FUNCTION setLogicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setLogicalObjectName,LOGICAL,INPUT c CHARACTER FUNCTION setInstanceProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setDynamicObject,LOGICAL,INPUT lTemp LOGICAL FUNCTION setDesignDataObject,LOGICAL,INPUT pcDataObject CHARACTER FUNCTION setDBAware,LOGICAL,INPUT lAware LOGICAL FUNCTION setDataTargetEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setDataTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setDataSourceNames,LOGICAL,INPUT pcSourceNames CHARACTER FUNCTION setDataSourceEvents,LOGICAL,INPUT pcEventsList CHARACTER FUNCTION setDataSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDataLinksEnabled,LOGICAL,INPUT lDataLinksEnabled LOGICAL FUNCTION setContainerSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setContainerSource,LOGICAL,INPUT phObject HANDLE FUNCTION setContainerHidden,LOGICAL,INPUT plHidden LOGICAL FUNCTION setChildDataKey,LOGICAL,INPUT cChildDataKey CHARACTER FUNCTION reviewMessages,CHARACTER, FUNCTION propertyType,CHARACTER,INPUT pcPropName CHARACTER FUNCTION messageNumber,CHARACTER,INPUT piMessage INTEGER FUNCTION mappedEntry,CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER FUNCTION linkProperty,CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER FUNCTION linkHandles,CHARACTER,INPUT pcLink CHARACTER FUNCTION instancePropertyList,CHARACTER,INPUT pcPropList CHARACTER FUNCTION getUserProperty,CHARACTER,INPUT pcPropName CHARACTER FUNCTION getUIBMode,CHARACTER, FUNCTION getTranslatableProperties,CHARACTER, FUNCTION getSupportedLinks,CHARACTER, FUNCTION getRunAttribute,CHARACTER, FUNCTION getQueryObject,LOGICAL, FUNCTION getPropertyDialog,CHARACTER, FUNCTION getPhysicalVersion,CHARACTER, FUNCTION getPhysicalObjectName,CHARACTER, FUNCTION getPassThroughLinks,CHARACTER, FUNCTION getParentDataKey,CHARACTER, FUNCTION getObjectVersionNumber,CHARACTER, FUNCTION getObjectVersion,CHARACTER, FUNCTION getObjectParent,HANDLE, FUNCTION getObjectPage,INTEGER, FUNCTION getObjectName,CHARACTER, FUNCTION getObjectInitialized,LOGICAL, FUNCTION getObjectHidden,LOGICAL, FUNCTION getLogicalVersion,CHARACTER, FUNCTION getLogicalObjectName,CHARACTER, FUNCTION getInstanceProperties,CHARACTER, FUNCTION getDynamicObject,LOGICAL, FUNCTION getDesignDataObject,CHARACTER, FUNCTION getDBAware,LOGICAL, FUNCTION getDataTargetEvents,CHARACTER, FUNCTION getDataTarget,CHARACTER, FUNCTION getDataSourceNames,CHARACTER, FUNCTION getDataSourceEvents,CHARACTER, FUNCTION getDataSource,HANDLE, FUNCTION getDataLinksEnabled,LOGICAL, FUNCTION getContainerType,CHARACTER, FUNCTION getContainerSourceEvents,CHARACTER, FUNCTION getContainerSource,HANDLE, FUNCTION getContainerHidden,LOGICAL, FUNCTION getContainerHandle,HANDLE, FUNCTION getChildDataKey,CHARACTER, FUNCTION fetchMessages,CHARACTER, FUNCTION assignLinkProperty,LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION anyMessage,LOGICAL, FUNCTION setServerOperatingMode,LOGICAL,INPUT pcServerOperatingMode CHARACTER FUNCTION setServerFileName,LOGICAL,INPUT pcFileName CHARACTER FUNCTION setASUsePrompt,LOGICAL,INPUT plFlag LOGICAL FUNCTION setASInitializeOnRun,LOGICAL,INPUT plInitialize LOGICAL FUNCTION setASInfo,LOGICAL,INPUT pcInfo CHARACTER FUNCTION setASHandle,LOGICAL,INPUT phASHandle HANDLE FUNCTION setASDivision,LOGICAL,INPUT pcDivision CHARACTER FUNCTION setAppService,LOGICAL,INPUT pcAppService CHARACTER FUNCTION runServerProcedure,HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE FUNCTION getServerOperatingMode,CHARACTER, FUNCTION getServerFileName,CHARACTER, FUNCTION getASUsePrompt,LOGICAL, FUNCTION getASInitializeOnRun,LOGICAL, FUNCTION getASInfo,CHARACTER, FUNCTION getASHasStarted,LOGICAL, FUNCTION getASHandle,HANDLE, FUNCTION getAsDivision,CHARACTER, FUNCTION getASBound,LOGICAL, FUNCTION getAppService,CHARACTER, FUNCTION createUiEvents,LOGICAL, FUNCTION getObjectSecured,LOGICAL, FUNCTION getObjectTranslated,LOGICAL, FUNCTION setResizeVertical,LOGICAL,INPUT plResizeVertical LOGICAL FUNCTION setResizeHorizontal,LOGICAL,INPUT plResizeHorizontal LOGICAL FUNCTION setObjectLayout,LOGICAL,INPUT pcLayout CHARACTER FUNCTION setLayoutOptions,LOGICAL,INPUT pcOptions CHARACTER FUNCTION setHideOnInit,LOGICAL,INPUT plHide LOGICAL FUNCTION setDisableOnInit,LOGICAL,INPUT plDisable LOGICAL FUNCTION setDefaultLayout,LOGICAL,INPUT pcDefault CHARACTER FUNCTION setAllFieldNames,LOGICAL,INPUT pcValue CHARACTER FUNCTION setAllFieldHandles,LOGICAL,INPUT pcValue CHARACTER FUNCTION getResizeVertical,LOGICAL, FUNCTION getResizeHorizontal,LOGICAL, FUNCTION getWidth,DECIMAL, FUNCTION getRow,DECIMAL, FUNCTION getObjectLayout,CHARACTER, FUNCTION getObjectEnabled,LOGICAL, FUNCTION getLayoutVariable,CHARACTER, FUNCTION getLayoutOptions,CHARACTER, FUNCTION getHideOnInit,LOGICAL, FUNCTION getHeight,DECIMAL, FUNCTION getEnabledObjHdls,CHARACTER, FUNCTION getEnabledObjFlds,CHARACTER, FUNCTION getDisableOnInit,LOGICAL, FUNCTION getDefaultLayout,CHARACTER, FUNCTION getCol,DECIMAL, FUNCTION getAllFieldNames,CHARACTER, FUNCTION getAllFieldHandles,CHARACTER, FUNCTION setStatusArea,LOGICAL,INPUT plStatusArea LOGICAL FUNCTION getObjectType,character, FUNCTION setWindowTitleViewer,LOGICAL,INPUT phViewer HANDLE FUNCTION setWaitForObject,LOGICAL,INPUT phObject HANDLE FUNCTION setUpdateTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setUpdateSource,LOGICAL,INPUT pcSource CHARACTER FUNCTION setTopOnly,LOGICAL,INPUT plTopOnly LOGICAL FUNCTION setSdoForeignFields,LOGICAL,INPUT cSdoForeignFields CHARACTER FUNCTION setSavedContainerMode,LOGICAL,INPUT cSavedContainerMode CHARACTER FUNCTION setRunMultiple,LOGICAL,INPUT plMultiple LOGICAL FUNCTION setRunDOOptions,LOGICAL,INPUT pcOptions CHARACTER FUNCTION setRouterTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setReEnableDataLinks,LOGICAL,INPUT cReEnableDataLinks CHARACTER FUNCTION setPrimarySdoTarget,LOGICAL,INPUT hPrimarySdoTarget HANDLE FUNCTION setPageSource,LOGICAL,INPUT phObject HANDLE FUNCTION setPageNTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setOutMessageTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setNavigationTarget,LOGICAL,INPUT cTarget CHARACTER FUNCTION setNavigationSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setNavigationSource,LOGICAL,INPUT pcSource CHARACTER FUNCTION setMultiInstanceSupported,LOGICAL,INPUT lMultiInstanceSupported LOGICAL FUNCTION setMultiInstanceActivated,LOGICAL,INPUT lMultiInstanceActivated LOGICAL FUNCTION setInMessageTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setFilterSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDynamicSDOProcedure,LOGICAL,INPUT pcProc CHARACTER FUNCTION setDisabledAddModeTabs,LOGICAL,INPUT cDisabledAddModeTabs CHARACTER FUNCTION setCurrentPage,LOGICAL,INPUT iPage INTEGER FUNCTION setContainerTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setContainerMode,LOGICAL,INPUT cContainerMode CHARACTER FUNCTION setCallerWindow,LOGICAL,INPUT h HANDLE FUNCTION setCallerProcedure,LOGICAL,INPUT h HANDLE FUNCTION setCallerObject,LOGICAL,INPUT h HANDLE FUNCTION pageNTargets,CHARACTER,INPUT phTarget HANDLE,INPUT piPageNum INTEGER FUNCTION getStatusArea,LOGICAL, FUNCTION getWindowTitleViewer,HANDLE, FUNCTION getWaitForObject,HANDLE, FUNCTION getUpdateTarget,CHARACTER, FUNCTION getUpdateSource,CHARACTER, FUNCTION getTopOnly,LOGICAL, FUNCTION getSdoForeignFields,CHARACTER, FUNCTION getSavedContainerMode,CHARACTER, FUNCTION getRunMultiple,LOGICAL, FUNCTION getRunDOOptions,CHARACTER, FUNCTION getReEnableDataLinks,CHARACTER, FUNCTION getPrimarySdoTarget,HANDLE, FUNCTION getPageSource,HANDLE, FUNCTION getPageNTarget,CHARACTER, FUNCTION getOutMessageTarget,HANDLE, FUNCTION getNavigationTarget,HANDLE, FUNCTION getNavigationSourceEvents,CHARACTER, FUNCTION getNavigationSource,CHARACTER, FUNCTION getMultiInstanceSupported,LOGICAL, FUNCTION getMultiInstanceActivated,LOGICAL, FUNCTION getFilterSource,HANDLE, FUNCTION getDynamicSDOProcedure,CHARACTER, FUNCTION getDisabledAddModeTabs,CHARACTER, FUNCTION getCurrentPage,INTEGER, FUNCTION getContainerTargetEvents,CHARACTER, FUNCTION getContainerTarget,CHARACTER, FUNCTION getContainerMode,CHARACTER, FUNCTION getCallerWindow,HANDLE, FUNCTION getCallerProcedure,HANDLE, FUNCTION enablePagesInFolder,LOGICAL,INPUT pcPageInformation CHARACTER FUNCTION disablePagesInFolder,LOGICAL,INPUT pcPageInformation CHARACTER FUNCTION widgetValueList,CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION widgetValue,CHARACTER,INPUT pcName CHARACTER FUNCTION widgetIsTrue,LOGICAL,INPUT pcName CHARACTER FUNCTION widgetIsModified,LOGICAL,INPUT pcNameList CHARACTER FUNCTION widgetIsFocused,LOGICAL,INPUT pcName CHARACTER FUNCTION widgetIsBlank,LOGICAL,INPUT pcNameList CHARACTER FUNCTION widgetLongcharValue,LONGCHAR,INPUT pcName CHARACTER FUNCTION widgetHandle,HANDLE,INPUT pcName CHARACTER FUNCTION viewWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION toggleWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION resetWidgetValue,LOGICAL,INPUT pcNameList CHARACTER FUNCTION highlightWidget,LOGICAL,INPUT pcNameList CHARACTER,INPUT pcHighlightType CHARACTER FUNCTION hideWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION formattedWidgetValueList,CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION formattedWidgetValue,CHARACTER,INPUT pcName CHARACTER FUNCTION enableWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION enableRadioButton,LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER FUNCTION disableWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION disableRadioButton,LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER FUNCTION clearWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION blankWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION assignWidgetValueList,LOGICAL,INPUT pcNameList CHARACTER,INPUT pcValueList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION assignWidgetValue,LOGICAL,INPUT pcName CHARACTER,INPUT pcValue CHARACTER FUNCTION assignFocusedWidget,LOGICAL,INPUT pcName CHARACTER        �/              P              �" �/  X�               �               /    +   p� �  7   � `  8   p� �   B   d� |  C   ��    D    � $  E   $� 	  F   (�   G   4� �&  H   �� t  I   <� �  J   ��   K           �� �  ? �� �'  iSO8859-1                                                                            /   ' �                                      �                  ��    $              �(     �(   �]   ��  `/         �  �   �/      �/          �                                             PROGRESS                         �           
    
                    �              �                                                                                                     
                                                                                                         �             �         �       �  L  .  �   �.  �   �   �ra�.         �             �*          P+      �   ,  .
      �  
    
                  �  \                                                                                                       .
          
  �  @
      T  
    
                  @               �                                                                                          @
          
  �  R
         
    
                  �  �             p                                                                                          R
          
  0  _
      �  
    
                  �  `                                                                                                       _
          
  �  r
      X  
    
                  D    	           �                                                                                          r
          
  �  �
        
    
                  �  �  
           t                                                                                          �
          
  4  �
      �  
    
                  �  d                                                                                                        �
          
  �  �
      \  
    
                  H               �                                                                                          �
          
  �  �
                               �  �             x                                                                                          �
            8	  �
      �                        �  h	             $	                                                                                          �
            �	  �
      `	  
    
                  L	  
             �	                                                                                          �
          
  �
  �
      
  
    
                  �	  �
             |
                                                                                          �
          
  <  �
      �
  
    
                  �
  l             (                                                                                          �
          
  �        d                        P               �                                                                                                      �                                �  �             �                                                                                                      @        �                        �  p             ,                                                                                                          .      h                        T  <             �                                                                                          .                         INTEGRAL                         PROGRESS                         p       �                               �#sa              g{                              �  �                      �  �  �      CODCIATABLALLAVE_C1LLAVE_C2LLAVE_C3RANGO_VALORVALORRANGO_FECHALLAVE_C4LLAVE_C5LLAVE_C6LIBRE_C02LIBRE_C03LIBRE_C01LLAVE_C7LLAVE_C8TASAIMPUESTOIMPORTEUNITARIOSINIMPUESTOIMPORTEUNITARIOIMPUESTO                                                                      	         
                                                                                                              �     F  �      F                         �#sa            O  �                              �  �                      P     P�     CODMATDESMATCODMARUNDSTKUNDCMPFACEQUCODCTACODNEWMONVTAPREVTAPREBASAFTIGVVINMN1CODCIAVINMN2CODFAMVCTMN1FCHACTCODPR1CODPR2VCTMN2ARTPROFCHUSALFCHUCMPPMAXMN1PMAXMN2PULTMN1PULTMN2USUARIOFCHINGFCHCESFCHALZCLFMATUNDBASSUBFAMCODBRRCODANTTIPARTFCHPRMDFCHPRMHFCHREAPESMATDETALLECANEMPALMACENESDESMARAFTISCPORISCPORVTATPOMRGCTOLISCTOPRMMRGUTIPORMAXFCHMPREUNDANTPREANTPREACTDSCTOSTPOPROPORIGVCTOTOTTPOSUMCTOUNDORDENORDLISORDTMPTPOARTTPOCMBPPCHR__01CHR__02CHR__03DEC__01DEC__02DEC__03DATE__01DATE__02DATE__03MRGUTI-AMRGUTI-BMRGUTI-CPREOFIUNDAUNDBUNDCFLGINTFLGPRECLASEFCHPROMCATCONTATIPROTDSCTOPROMINFORFLGINFORPROMDIVIPROMFCHDPROMFCHHPROMDTODTOVOLRDTOVOLDUNDALTDSCALTMRGALTPREALTLICENCIAPROMMINDIVIPROMMINFCHDPROMMINFCHHPROMMINDTOCODDIGESAVTODIGESALIBRE_C01LIBRE_C02LIBRE_C03LIBRE_C04LIBRE_C05LIBRE_D01LIBRE_D02LIBRE_F01LIBRE_F02STKMINSTKMAXSTKREPDESCRIPCION-LARGADESCRIPCION-TECNICASW-WEBWEB-SUBCATEGORIALIBRE_D03LIBRE_D04LIBRE_D05PESOBRUTOPAQUETELARGOALTOANCHOCTOLISMARCOCTOTOTMARCOCODSSFAMCLFESPECIALFLGCOMERCIALLIBRE_C06LIBRE_C07LIBRE_C08LIBRE_C09LIBRE_C10CODIGOPADREFACTORPADREREQUIERESERIALNRREQUIEREDUEDATEDTOVOLPTASAIMPUESTOIMPORTEUNITARIOSINIMPUESTODTOVOLPSINIMPUESTOIMPORTEUNITARIOSINIMPUESTO_AIMPORTEUNITARIOSINIMPUESTO_BIMPORTEUNITARIOSINIMPUESTO_CDTOVOLPIMPUESTOIMPORTEUNITARIOIMPUESTOIMPORTEUNITARIOIMPUESTO_AIMPORTEUNITARIOIMPUESTO_BIMPORTEUNITARIOIMPUESTO_C                                                                      	          
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
        �          �          �          �          <     |  �      |                         �ɺ[            |  W�                              �  h                      �  x  > 	     CODDIVFPROCESOTOTCOTIZNDIASMETAFDESDEFHASTADIV-VTASNDIVISIONES                                                                        	 2        
              A      H          <      �%  �      �%                         �ɺ[            �%  �e                              �  �                      �  �  %      TABLACODIGONOMBRENOMANTCODCTA1CODCTA2                                                               `!      Q&  �      Q&                          �ra            Z&  ��  I                           �  �                        !  �   1      CODCIATABLACODIGONOMBREVALORCAMPO-CCAMPO-DCAMPO-L                                                                      	         �"  !   Q&  �      w&   C                       �ra            w&  ��  I                           �  �!                      $"  �!  1      CODCIATABLACODIGONOMBREVALORCAMPO-CCAMPO-DCAMPO-L                                                                      	         #  #   Q&  �      Q&                         �ra            Z&  ��                              �  �                       `&  %   3'  �      3'                         �ɺ[            3'  .'                              �  �#                      �$  �#  %     CODCIACODALMCODMATUNDVTACODUBISTKACTSTKMINSTKMAXSTKREPSTKINIVINMN1VINMN2VCTMN1VCTMN2FCHINGFCHSALFCHINVSELINVFACEQUDESMATALMDESCODMARCODANTSTKACTCBDLIBRE_C01LIBRE_C02LIBRE_C03LIBRE_C04LIBRE_C05LIBRE_D01LIBRE_D02LIBRE_F01LIBRE_F02STKCOMPROMETIDOSTOCKMAXSTOCKSEGSTOCKMAXSEG                                                                        	          
                                                                                                                                                                                                                                       !          "          #          $          %          &              &   L'  �      L'                         C(�\            L'  ��                              �  �&                      �'  �&  �      CODALMDESCRIPCIONCODCIATDOARTAUTMOVDIRALMHORRECENCALMTELALMCORRSALCORRINGCORRTRFCODDIVCLAVEALMCSGTPOCSGCODCLIFLGREPCAMPO-CALMPRINCIPALALMDESPACHOCAMPO-LOG                                                                        	          
                                                                                                     
                             
                      8�                                               <�          *  T*  H X�(            
                                                       
             
             
                                         
                                                                                                                H   X   h   x   �   �   �   �   �   �   �   �       (  8  H      H   X   h   x   �   �   �   �   �   �   �   �      (  8  H                                                                                                                                 	                                 �,  �,  �,  �,  �,          �,             -  -   -  (-                             ,-  4-  <-  L-  D-          P-             d-  l-  t-  �-  |-          �-              �-  �-  �-  �-                             �-  �-  �-  �-                              �-  �-  �-  �-                              �-  .  .  .                                                                          CodCia  999 Codigo  Codigo  0   Codigo de compania  Tabla   x(2)    Tabla       Codigo  x(8)    Codigo  Codigo      C�digo de auxiliar  Nombre  x(40)   Nombre  Nombre      Nombre de auxiliar  Valor   ->>,>>9.9999    Valor   0   Campo-C x(8)    Campo-C     Campo-D 99/99/99    Campo-D ?   Campo-L yes/no  Campo-L no  �  ���	������    � �                      �� �                      �� �  ���������������������� �                      �       i'                �     i  i  i     	 	 	    �  �  �  �  �  �  �       ��                                               7          ����                            i'   zA    i'         �'   ��    �'   �    �'   �y    i'  # �    �'  % y�    �'  & !�    undefined                                                               �       T�  �   l   d�    ��                  �����               |�                    O   ����    e�          O   ����    R�          O   ����    ��      t       �   �              4   ����     /                                    3   ����       $     H  ���                       8      
                       � ߱        �  �      D       p
     7          assignFocusedWidget         �      �            LOGICAL,INPUT pcName CHARACTER  assignWidgetValue   �      �      $    $       LOGICAL,INPUT pcName CHARACTER,INPUT pcValue CHARACTER  assignWidgetValueList         \      �    6       LOGICAL,INPUT pcNameList CHARACTER,INPUT pcValueList CHARACTER,INPUT pcDelimiter CHARACTER  blankWidget t      �          L       LOGICAL,INPUT pcNameList CHARACTER  clearWidget �      @      l    X       LOGICAL,INPUT pcNameList CHARACTER  disableRadioButton  L      �      �    d       LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER    disableWidget   �            4    w       LOGICAL,INPUT pcNameList CHARACTER  enableRadioButton         X      �    �       LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER    enableWidget    l      �      �    �       LOGICAL,INPUT pcNameList CHARACTER  formattedWidgetValue    �             X  	  �       CHARACTER,INPUT pcName CHARACTER    formattedWidgetValueList    8      |      �  
  �       CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER    hideWidget  �      �      (   
 �       LOGICAL,INPUT pcNameList CHARACTER  highlightWidget       L      |    �       LOGICAL,INPUT pcNameList CHARACTER,INPUT pcHighlightType CHARACTER  resetWidgetValue    \      �      �    �       LOGICAL,INPUT pcNameList CHARACTER  toggleWidget    �            H    �       LOGICAL,INPUT pcNameList CHARACTER  viewWidget  (      l      �   
       LOGICAL,INPUT pcNameList CHARACTER  widgetHandle    x      �      �          HANDLE,INPUT pcName CHARACTER   widgetLongcharValue �            @    #      LONGCHAR,INPUT pcName CHARACTER widgetIsBlank          `      �    7      LOGICAL,INPUT pcNameList CHARACTER  widgetIsFocused p      �      �    E      LOGICAL,INPUT pcName CHARACTER  widgetIsModified    �      	      8	    U      LOGICAL,INPUT pcNameList CHARACTER  widgetIsTrue    	      \	      �	    f      LOGICAL,INPUT pcName CHARACTER  widgetValue l	      �	      �	    s      CHARACTER,INPUT pcName CHARACTER    widgetValueList �	      �	      ,
          CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER    �    ;  �
  �
  P  d       4   ����d       o   <       �
                              �  �   NA  �   �  �   �  �      �      �     �         $    8    L  `  `  
`  t  $  �    �     �      $  M  |  ���                       �     
                    � ߱        ؁    |  �  @      �      4   �����                P                      ��                  }  �                  Xl                       }  �  �      l  |             4   ����       $  �  �  ���                       P  @         <              � ߱              �  �         �      4   �����      $  �  ,  ���                       �  @         �              � ߱        assignPageProperty                              �  �      ��                                    $�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   T                             ��                  H           ��                            ����                            changePage                              @  (      ��                  	  
  X              �                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            confirmExit                             @  (      ��                      X              ,�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  p           ��                            ����                            constructObject                             l  T      ��                      �              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             �               �� 
  �             �  
             ��                 �               �� 
                   
         ��                            ����                            createObjects                                 �      ��                      (              �                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            deletePage                                �      ��                      (              ă                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  @           ��                            ����                            destroyObject                               <  $      ��                      T              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hidePage                                <  $      ��                  !  #  T              L�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  l           ��                            ����                            initializeObject                                l  T      ��                  %  &  �              �&                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeVisualContainer                               |  d      ��                  (  )  �              T)                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initPages                               |  d      ��                  +  -  �              *                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            notifyPage                              �  �      ��                  /  1  �              �                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            passThrough                             �  �      ��                  3  6  �              �                    O   ����    e�          O   ����    R�          O   ����    ��            ��   0             �               ��                  $           ��                            ����                            removePageNTarget                               $        ��                  8  ;  <              ��                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �             T  
             ��                  |           ��                            ����                            selectPage                              t  \      ��                  =  ?  �              �                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            toolbar                             �  �      ��                  A  C  �              \=                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            viewObject                              �   �       ��                  E  F  �               �                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            viewPage                                �!  �!      ��                  H  J  �!              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �!           ��                            ����                            disablePagesInFolder    
      X"      �"    �      LOGICAL,INPUT pcPageInformation CHARACTER   enablePagesInFolder p"      �"      �"    �      LOGICAL,INPUT pcPageInformation CHARACTER   getCallerProcedure  �"      #      P#    �      HANDLE, getCallerWindow 0#      X#      �#    �      HANDLE, getContainerMode    h#      �#      �#    �      CHARACTER,  getContainerTarget  �#      �#      $    �      CHARACTER,  getContainerTargetEvents    �#      $      L$    �      CHARACTER,  getCurrentPage  ,$      X$      �$          INTEGER,    getDisabledAddModeTabs  h$      �$      �$            CHARACTER,  getDynamicSDOProcedure  �$      �$      %  !  7      CHARACTER,  getFilterSource �$      %      L%  "  N      HANDLE, getMultiInstanceActivated   ,%      T%      �%  #  ^      LOGICAL,    getMultiInstanceSupported   p%      �%      �%  $  x      LOGICAL,    getNavigationSource �%      �%      &  %  �      CHARACTER,  getNavigationSourceEvents   �%      $&      `&  &  �      CHARACTER,  getNavigationTarget @&      l&      �&  '  �      HANDLE, getOutMessageTarget �&      �&      �&  (  �      HANDLE, getPageNTarget  �&      �&      '  )  �      CHARACTER,  getPageSource   �&       '      P'  *  �      HANDLE, getPrimarySdoTarget 0'      X'      �'  +        HANDLE, getReEnableDataLinks    l'      �'      �'  ,        CHARACTER,  getRunDOOptions �'      �'      (  -  .      CHARACTER,  getRunMultiple  �'      (      D(  .  >      LOGICAL,    getSavedContainerMode   $(      P(      �(  /  M      CHARACTER,  getSdoForeignFields h(      �(      �(  0  c      CHARACTER,  getTopOnly  �(      �(       )  1 
 w      LOGICAL,    getUpdateSource �(      )      <)  2  �      CHARACTER,  getUpdateTarget )      H)      x)  3  �      CHARACTER,  getWaitForObject    X)      �)      �)  4  �      HANDLE, getWindowTitleViewer    �)      �)      �)  5  �      HANDLE, getStatusArea   �)       *      0*  6  �      LOGICAL,    pageNTargets    *      <*      l*  7  �      CHARACTER,INPUT phTarget HANDLE,INPUT piPageNum INTEGER setCallerObject L*      �*      �*  8  �      LOGICAL,INPUT h HANDLE  setCallerProcedure  �*      �*       +  9  �      LOGICAL,INPUT h HANDLE  setCallerWindow  +      8+      h+  :        LOGICAL,INPUT h HANDLE  setContainerMode    H+      �+      �+  ;        LOGICAL,INPUT cContainerMode CHARACTER  setContainerTarget  �+      �+      ,  <  '      LOGICAL,INPUT pcObject CHARACTER    setCurrentPage  �+      4,      d,  =  :      LOGICAL,INPUT iPage INTEGER setDisabledAddModeTabs  D,      �,      �,  >  I      LOGICAL,INPUT cDisabledAddModeTabs CHARACTER    setDynamicSDOProcedure  �,      �,       -  ?  `      LOGICAL,INPUT pcProc CHARACTER  setFilterSource  -      @-      p-  @  w      LOGICAL,INPUT phObject HANDLE   setInMessageTarget  P-      �-      �-  A  �      LOGICAL,INPUT phObject HANDLE   setMultiInstanceActivated   �-      �-       .  B  �      LOGICAL,INPUT lMultiInstanceActivated LOGICAL   setMultiInstanceSupported    .      P.      �.  C  �      LOGICAL,INPUT lMultiInstanceSupported LOGICAL   setNavigationSource l.      �.      �.  D  �      LOGICAL,INPUT pcSource CHARACTER    setNavigationSourceEvents   �.      /      P/  E  �      LOGICAL,INPUT pcEvents CHARACTER    setNavigationTarget 0/      t/      �/  F  �      LOGICAL,INPUT cTarget CHARACTER setOutMessageTarget �/      �/      �/  G        LOGICAL,INPUT phObject HANDLE   setPageNTarget  �/      0      L0  H  $      LOGICAL,INPUT pcObject CHARACTER    setPageSource   ,0      p0      �0  I  3      LOGICAL,INPUT phObject HANDLE   setPrimarySdoTarget �0      �0      �0  J  A      LOGICAL,INPUT hPrimarySdoTarget HANDLE  setReEnableDataLinks    �0      1      T1  K  U      LOGICAL,INPUT cReEnableDataLinks CHARACTER  setRouterTarget 41      �1      �1  L  j      LOGICAL,INPUT phObject HANDLE   setRunDOOptions �1      �1       2  M  z      LOGICAL,INPUT pcOptions CHARACTER   setRunMultiple  �1      $2      T2  N  �      LOGICAL,INPUT plMultiple LOGICAL    setSavedContainerMode   42      x2      �2  O  �      LOGICAL,INPUT cSavedContainerMode CHARACTER setSdoForeignFields �2      �2      3  P  �      LOGICAL,INPUT cSdoForeignFields CHARACTER   setTopOnly  �2      <3      h3  Q 
 �      LOGICAL,INPUT plTopOnly LOGICAL setUpdateSource H3      �3      �3  R  �      LOGICAL,INPUT pcSource CHARACTER    setUpdateTarget �3      �3      4  S  �      LOGICAL,INPUT pcTarget CHARACTER    setWaitForObject    �3      04      d4  T  �      LOGICAL,INPUT phObject HANDLE   setWindowTitleViewer    D4      �4      �4  U  �      LOGICAL,INPUT phViewer HANDLE   getObjectType   �4      �4      5  V        CHARACTER,  setStatusArea   �4      5      H5  W  "      LOGICAL,INPUT plStatusArea LOGICAL  applyLayout                             �5  �5      ��                  �  �  6              �	�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disableObject                                7  �6      ��                  �  �  7              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            enableObject                                8  �7      ��                  �  �  8              0�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                9  �8      ��                  �  �  $9              �ߕ                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            processAction                               :  �9      ��                  �  �  (:              ���                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  @:           ��                            ����                            getAllFieldHandles  (5      �:      �:  X  0      CHARACTER,  getAllFieldNames    �:      �:      ;  Y  C      CHARACTER,  getCol  �:      (;      P;  Z  T      DECIMAL,    getDefaultLayout    0;      \;      �;  [  [      CHARACTER,  getDisableOnInit    p;      �;      �;  \  l      LOGICAL,    getEnabledObjFlds   �;      �;      <  ]  }      CHARACTER,  getEnabledObjHdls   �;      <      P<  ^  �      CHARACTER,  getHeight   0<      \<      �<  _ 	 �      DECIMAL,    getHideOnInit   h<      �<      �<  `  �      LOGICAL,    getLayoutOptions    �<      �<      =  a  �      CHARACTER,  getLayoutVariable   �<      =      D=  b  �      CHARACTER,  getObjectEnabled    $=      P=      �=  c  �      LOGICAL,    getObjectLayout d=      �=      �=  d  �      CHARACTER,  getRow  �=      �=      �=  e  �      DECIMAL,    getWidth    �=       >      ,>  f        DECIMAL,    getResizeHorizontal >      8>      l>  g        LOGICAL,    getResizeVertical   L>      x>      �>  h  !      LOGICAL,    setAllFieldHandles  �>      �>      �>  i  3      LOGICAL,INPUT pcValue CHARACTER setAllFieldNames    �>      ?      @?  j  F      LOGICAL,INPUT pcValue CHARACTER setDefaultLayout     ?      `?      �?  k  W      LOGICAL,INPUT pcDefault CHARACTER   setDisableOnInit    t?      �?      �?  l  h      LOGICAL,INPUT plDisable LOGICAL setHideOnInit   �?      @      <@  m  y      LOGICAL,INPUT plHide LOGICAL    setLayoutOptions    @      \@      �@  n  �      LOGICAL,INPUT pcOptions CHARACTER   setObjectLayout p@      �@      �@  o  �      LOGICAL,INPUT pcLayout CHARACTER    setResizeHorizontal �@      A      <A  p  �      LOGICAL,INPUT plResizeHorizontal LOGICAL    setResizeVertical   A      hA      �A  q  �      LOGICAL,INPUT plResizeVertical LOGICAL  getObjectTranslated |A      �A      �A  r  �      LOGICAL,    getObjectSecured    �A      B      8B  s  �      LOGICAL,    createUiEvents  B      DB      tB  t  �      LOGICAL,    bindServer                              C  �B      ��                  �  �  (C               ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               D  �C      ��                  �  �  ,D              ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyServerObject                             E  E      ��                  �  �  4E              dӕ                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disconnectObject                                $F  F      ��                  �  �  <F              ԕ                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              0G  G      ��                  �  �  HG              0@�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            restartServerObject                             8H   H      ��                  �  �  PH              �@�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            runServerObject                             <I  $I      ��                  �  �  TI              4A�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 lI  
         ��                            ����                            startServerObject                               lJ  TJ      ��                  �  �  �J              H��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            unbindServer                                pK  XK      ��                  �  �  �K              �Z�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �K           ��                            ����                            getAppService   TB      L      8L  u  	      CHARACTER,  getASBound  L      DL      pL  v 
 	      LOGICAL,    getAsDivision   PL      |L      �L  w  	      CHARACTER,  getASHandle �L      �L      �L  x  )	      HANDLE, getASHasStarted �L      �L      M  y  5	      LOGICAL,    getASInfo   �L      (M      TM  z 	 E	      CHARACTER,  getASInitializeOnRun    4M      `M      �M  {  O	      LOGICAL,    getASUsePrompt  xM      �M      �M  |  d	      LOGICAL,    getServerFileName   �M      �M      N  }  s	      CHARACTER,  getServerOperatingMode  �M       N      XN  ~  �	      CHARACTER,  runServerProcedure  8N      dN      �N    �	      HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE   setAppService   xN      �N      O  �  �	      LOGICAL,INPUT pcAppService CHARACTER    setASDivision   �N      4O      dO  �  �	      LOGICAL,INPUT pcDivision CHARACTER  setASHandle DO      �O      �O  �  �	      LOGICAL,INPUT phASHandle HANDLE setASInfo   �O      �O       P  � 	 �	      LOGICAL,INPUT pcInfo CHARACTER  setASInitializeOnRun    �O       P      XP  �  �	      LOGICAL,INPUT plInitialize LOGICAL  setASUsePrompt  8P      |P      �P  �  �	      LOGICAL,INPUT plFlag LOGICAL    setServerFileName   �P      �P       Q  �  
      LOGICAL,INPUT pcFileName CHARACTER  setServerOperatingMode  �P      $Q      \Q  �  
      LOGICAL,INPUT pcServerOperatingMode CHARACTER   addLink                             R   R      ��                  �  �  0R              t0�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  |R             HR  
             ��   �R             pR               �� 
                 �R  
         ��                            ����                            addMessage                              �S  xS      ��                  �  �  �S              䓔                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �S             �S               ��   T             �S               ��                  T           ��                            ����                            adjustTabOrder                              U  �T      ��                  �  �  $U              ���                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  pU             <U  
             �� 
  �U             dU  
             ��                  �U           ��                            ����                            applyEntry                              �V  lV      ��                  �  �  �V              \P�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �V           ��                            ����                            changeCursor                                �W  �W      ��                  �  �  �W              �`�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �W           ��                            ����                            createControls                              �X  �X      ��                  �  �  �X              �                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �Y  �Y      ��                  �  �  �Y              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            displayLinks                                �Z  �Z      ��                  �  �  �Z              �                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            editInstanceProperties                              �[  �[      ��                  �  �  \              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            exitObject                              �\  �\      ��                  �  �  ]              �v�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hideObject                              �]  �]      ��                  �  �  ^              pw�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �^  �^      ��                  �  �  _              (@�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            modifyListProperty                               `  �_      ��                  �  �  `              �@�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  d`             0`  
             ��   �`             X`               ��   �`             �`               ��                  �`           ��                            ����                            modifyUserLinks                             �a  �a      ��                  �  �  �a              �ʒ                    O   ����    e�          O   ����    R�          O   ����    ��            ��   b             �a               ��   0b             �a               �� 
                 $b  
         ��                            ����                            removeAllLinks                               c  c      ��                  �  �  8c              |ۓ                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            removeLink                               d  d      ��                  �  �  8d              �ۓ                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �d             Pd  
             ��   �d             xd               �� 
                 �d  
         ��                            ����                            repositionObject                                �e  �e      ��                  �  �  �e              `ܓ                    O   ����    e�          O   ����    R�          O   ����    ��            ��   f             �e               ��                  �e           ��                            ����                            returnFocus                             �f  �f      ��                  �  �  g              $��                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                  g  
         ��                            ����                            showMessageProcedure                                $h  h      ��                  �  �  <h              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �h             Th               ��                  |h           ��                            ����                            toggleData                              ti  \i      ��                  �  �  �i              ���                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �i           ��                            ����                            viewObject                              �j  �j      ��                  �  �  �j              DE�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            anyMessage  <Q      k      8k  � 
 |      LOGICAL,    assignLinkProperty  k      Dk      xk  �  �      LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER   fetchMessages   Xk      �k       l  �  �      CHARACTER,  getChildDataKey �k      l      <l  �  �      CHARACTER,  getContainerHandle  l      Hl      |l  �  �      HANDLE, getContainerHidden  \l      �l      �l  �  �      LOGICAL,    getContainerSource  �l      �l      �l  �  �      HANDLE, getContainerSourceEvents    �l       m      <m  �  �      CHARACTER,  getContainerType    m      Hm      |m  �  
      CHARACTER,  getDataLinksEnabled \m      �m      �m  �        LOGICAL,    getDataSource   �m      �m      �m  �  /      HANDLE, getDataSourceEvents �m       n      4n  �  =      CHARACTER,  getDataSourceNames  n      @n      tn  �  Q      CHARACTER,  getDataTarget   Tn      �n      �n  �  d      CHARACTER,  getDataTargetEvents �n      �n      �n  �  r      CHARACTER,  getDBAware  �n      �n      (o  � 
 �      LOGICAL,    getDesignDataObject o      4o      ho  �  �      CHARACTER,  getDynamicObject    Ho      to      �o  �  �      LOGICAL,    getInstanceProperties   �o      �o      �o  �  �      CHARACTER,  getLogicalObjectName    �o      �o      0p  �  �      CHARACTER,  getLogicalVersion   p      <p      pp  �  �      CHARACTER,  getObjectHidden Pp      |p      �p  �  �      LOGICAL,    getObjectInitialized    �p      �p      �p  �        LOGICAL,    getObjectName   �p      �p      ,q  �        CHARACTER,  getObjectPage   q      8q      hq  �  &      INTEGER,    getObjectParent Hq      tq      �q  �  4      HANDLE, getObjectVersion    �q      �q      �q  �  D      CHARACTER,  getObjectVersionNumber  �q      �q      $r  �  U      CHARACTER,  getParentDataKey    r      0r      dr  �  l      CHARACTER,  getPassThroughLinks Dr      pr      �r  �  }      CHARACTER,  getPhysicalObjectName   �r      �r      �r  �  �      CHARACTER,  getPhysicalVersion  �r      �r      (s  �  �      CHARACTER,  getPropertyDialog   s      4s      hs  �  �      CHARACTER,  getQueryObject  Hs      ts      �s  �  �      LOGICAL,    getRunAttribute �s      �s      �s  �  �      CHARACTER,  getSupportedLinks   �s      �s       t  �  �      CHARACTER,  getTranslatableProperties    t      ,t      ht  �  �      CHARACTER,  getUIBMode  Ht      tt      �t  � 
       CHARACTER,  getUserProperty �t      �t      �t  �  "      CHARACTER,INPUT pcPropName CHARACTER    instancePropertyList    �t      u      <u  �  2      CHARACTER,INPUT pcPropList CHARACTER    linkHandles u      du      �u  �  G      CHARACTER,INPUT pcLink CHARACTER    linkProperty    pu      �u      �u  �  S      CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER mappedEntry �u       v      Lv  �  `      CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER  messageNumber   ,v      �v      �v  �  l      CHARACTER,INPUT piMessage INTEGER   propertyType    �v      w      <w  �  z      CHARACTER,INPUT pcPropName CHARACTER    reviewMessages  w      dw      �w  �  �      CHARACTER,  setChildDataKey tw      �w      �w  �  �      LOGICAL,INPUT cChildDataKey CHARACTER   setContainerHidden  �w      �w      ,x  �  �      LOGICAL,INPUT plHidden LOGICAL  setContainerSource  x      Lx      �x  �  �      LOGICAL,INPUT phObject HANDLE   setContainerSourceEvents    `x      �x      �x  �  �      LOGICAL,INPUT pcEvents CHARACTER    setDataLinksEnabled �x       y      4y  �  �      LOGICAL,INPUT lDataLinksEnabled LOGICAL setDataSource   y      \y      �y  �  �      LOGICAL,INPUT phObject HANDLE   setDataSourceEvents ly      �y      �y  �        LOGICAL,INPUT pcEventsList CHARACTER    setDataSourceNames  �y      z      <z  �        LOGICAL,INPUT pcSourceNames CHARACTER   setDataTarget   z      dz      �z  �  .      LOGICAL,INPUT pcTarget CHARACTER    setDataTargetEvents tz      �z      �z  �  <      LOGICAL,INPUT pcEvents CHARACTER    setDBAware  �z      {      <{  � 
 P      LOGICAL,INPUT lAware LOGICAL    setDesignDataObject {      \{      �{  �  [      LOGICAL,INPUT pcDataObject CHARACTER    setDynamicObject    p{      �{      �{  �  o      LOGICAL,INPUT lTemp LOGICAL setInstanceProperties   �{      |      @|  �  �      LOGICAL,INPUT pcPropList CHARACTER  setLogicalObjectName     |      d|      �|  �  �      LOGICAL,INPUT c CHARACTER   setLogicalVersion   ||      �|      �|  �  �      LOGICAL,INPUT cVersion CHARACTER    setObjectName   �|      }      @}  �  �      LOGICAL,INPUT pcName CHARACTER  setObjectParent  }      `}      �}  �  �      LOGICAL,INPUT phParent HANDLE   setObjectVersion    p}      �}      �}  �  �      LOGICAL,INPUT cObjectVersion CHARACTER  setParentDataKey    �}      ~      @~  �  �      LOGICAL,INPUT cParentDataKey CHARACTER  setPassThroughLinks  ~      h~      �~  �  �      LOGICAL,INPUT pcLinks CHARACTER setPhysicalObjectName   |~      �~      �~  �        LOGICAL,INPUT cTemp CHARACTER   setPhysicalVersion  �~            H  �  '      LOGICAL,INPUT cVersion CHARACTER    setRunAttribute (      l      �  �  :      LOGICAL,INPUT cRunAttribute CHARACTER   setSupportedLinks   |      �      �  �  J      LOGICAL,INPUT pcLinkList CHARACTER  setTranslatableProperties   �      �      X�  �  \      LOGICAL,INPUT pcPropList CHARACTER  setUIBMode  8�      |�      ��  � 
 v      LOGICAL,INPUT pcMode CHARACTER  setUserProperty ��      Ȁ      ��  �  �      LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER  showmessage ؀      8�      d�  �  �      LOGICAL,INPUT pcMessage CHARACTER   Signature   D�      ��      ��  � 	 �      CHARACTER,INPUT pcName CHARACTER    ��      �  p�            4   ����                ��                      ��                    0                  �|�                         �          ��  �      (      4   ����(                (�                      ��                    /                  X}�                         ��  (�      D�  ��      <      4   ����<                Ѓ                      ��                  (  *                  tV�                       (  T�         )                                  �     
                    � ߱        T�  $  ,  ��  ���                           $  .  ��  ���                       $       	       	           � ߱        ��    4  Ȅ  D�      4      4   ����4                T�                      ��                  5  �                  (W�                       5  ؄  ��  o   8      ,                                 ��  $   9  ��  ���                       �  @         �              � ߱        �  �   :  �      �  �   ;  <      �  �   =  �      0�  �   ?  $      D�  �   A  �      X�  �   C        l�  �   D  �      ��  �   E  �      ��  �   H  8      ��  �   J  �      ��  �   K  (	      І  �   M  �	      �  �   N   
      ��  �   O  \
      �  �   P  �
       �  �   Q  L      4�  �   W  �      H�  �   Y  �      \�  �   _  8      p�  �   a  �      ��  �   c         ��  �   d  �      ��  �   j        ��  �   k  �      ԇ  �   l        �  �   m  |      ��  �   p  �      �  �   q  ,      $�  �   s  �      8�  �   t  �      L�  �   v  P      `�  �   w  �      t�  �   x  �      ��  �   y        ��  �   z  @      ��  �   {  �      Ĉ  �   |  �      ؈  �   ~  4      �  �     p       �  �   �  �      �  �   �  �      (�  �   �  $      <�  �   �  `      P�  �   �  �          �   �  �                      |�          �  Љ      ��                   	  N	   �              �ݓ                    O   ����    e�          O   ����    R�          O   ����    ��      H     
                �       
       
       �                         � ߱        ��  $ 4	  �  ���                           O   L	  ��  ��                 �          �  �    �                                             ��                            ����                                �4      d�      ��     6     �                      V �                       x�    n	  ԋ  P�             4   ����                 `�                      ��                  o	  �	                  �A�                       o	  �  t�  �   r	  �      ��  �   s	  �      ��  �   t	  p      ��  �   u	  �      Č  �   v	  h      ،  �   w	  �      �  �   x	  X       �  �   y	  �      �  �   z	  P      (�  �   {	  �      <�  �   |	  @      P�  �   }	  �      d�  �   ~	  8          �   	  �      P�     
  ��  �      $      4   ����$                 �                      ��                  
  �
                  ���                       
  ��  4�  �   
  �      H�  �   
  �      \�  �   
  l      p�  �   
  �      ��  �   
  \       ��  �   
  �       ��  �   	
  L!      ��  �   

  �!      Ԏ  �   
  4"      �  �   
  �"      ��  �   
  $#      �  �   
  �#      $�  �   
  $      8�  �   
  �$      L�  �   
  %      `�  �   
  �%      t�  �   
  �%      ��  �   
  x&      ��  �   
  �&      ��  �   
  p'      ď  �   
  �'      ؏  �   
  h(      �  �   
  �(       �  �   
  `)      �  �   
  �)      (�  �   
  X*      <�  �   
  �*          �   
  P+      l�    �
  l�  �      �+      4   �����+                ��                      ��                  �
  M                   ��                       �
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
  �@      ĕ  $  Y  ��  ���                       A     
                    � ߱        \�    �  ��  �      A      4   ����A      /   �  �     ,�                          3   ����$A            L�                      3   ����DA  ��    �  x�  ��  ��  `A      4   ����`A  	              �                      ��             	     �  !                  L8�                       �  ��  �  �   �  �A      p�  $  �  D�  ���                       �A     
                    � ߱        ��  �   �  B      ܗ  $   �  ��  ���                       4B  @          B              � ߱        ��  $  �  �  ���                       �B                         � ߱        �B     
                xC       
       
       �D  @        
 �D              � ߱        (�  V   �  4�  ���                        �D                     E                     DE                         � ߱        ��  $  �  Ę  ���                       F     
                �F       
       
       �G  @        
 �G              � ߱        H�  V   �  T�  ���                        �G     
                XH       
       
       �I  @        
 hI              � ߱            V     �  ���                        
              ��                      ��             
     #  �                  䐕                       #  t�  �I     
                0J       
       
       �K  @        
 @K          �K  @        
 �K          DL  @        
 L          �L  @        
 dL              � ߱            V   8  �  ���                        adm-clone-props \�  ԛ              �     7     `                          \  �                     start-super-proc    �  @�  �           �     8                                                       H�    �  ̜  ܜ      0P      4   ����0P      /   �  �     �                          3   ����@P            8�                      3   ����`P  ��  $  �  t�  ���                       �P                         � ߱        \�      ��  8�  ؞  �P      4   �����P                ��                      ��                                      �u�                         ̝  �P                     �P                     �P                         � ߱            $    H�  ���                             	  ��  0�      �P      4   �����P  Q                         � ߱            $  
  �  ���                       X�      x�  ��  ��  $Q      4   ����$Q      $    ��  ���                       DQ                         � ߱            �   /  XQ      �Q     
                R       
       
       dS  @        
 $S              � ߱        ��  V   C  ��  ���                        ��  �   v  pS      0�    �  ��  Ġ      �S      4   �����S      /   �  �      �                          3   �����S             �                      3   �����S  �  $  �  \�  ���                       �S                         � ߱        (T     
                �T       
       
       �U  @        
 �U              � ߱        �  V     ��  ���                        ��    �  4�  ��       V      4   ���� V                ��                      ��                  �  �                  �ג                       �  D�      g   �  آ         ����                           ��          p�  X�      ��                  �      ��              �ג                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  ̣     ܣ  (V                      3   ����V  �     
   ��                      3   ����4V         
   ,�                      3   ����<V    ��                              ��        7                  ����                                        �              9      <�                      g                                �  g   �  �          ��	��                           إ          ��  ��      ��                  �  �  ��              �ڒ                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  �     �  `V                      3   ����DV            4�                      3   ����hV    ��                              ��        7                  ����                                        $�              :      D�                      g                               �  g   �  �          ��	��                           �          ��  ��      ��                  �  �  ȧ              xf�                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  �     �  �V                      3   �����V            <�                      3   �����V    ��                              ��        7                  ����                                        ,�              ;      L�                      g                               h�    �  $�  ��      �V      4   �����V                ��                      ��                  �  �                  $g�                       �  4�  �  /   �  ܩ     �                          3   �����V            �                      3   �����V  �  /  �  H�     X�  0W                      3   ����W  ��     
   x�                      3   ����8W  ��        ��                      3   ����@W  �        ت                      3   ����TW            �                      3   ����xW  @�    �  4�  D�      �W      4   �����W      /  �  p�     ��  $X                      3   ����X  ��     
   ��                      3   ����,X  �        Ы                      3   ����4X  �         �                      3   ����HX            0�                      3   ����lX        �  \�  l�      �X      4   �����X      /  �  ��     ��  �X                      3   �����X  ج     
   Ȭ                      3   �����X  �        ��                      3   �����X  8�        (�                      3   ����Y            X�                      3   ���� Y  (�    �  ��   �      DY      4   ����DY                �                      ��                  �  �                  4��                       �  ��      g   �  (�         ��̯        TY                  �          ��  ��      ��                  �      خ              ���                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  �     ,�  xY                      3   ����`Y  \�     
   L�                      3   �����Y         
   |�                      3   �����Y    ��                            ����                                        <�              <      ��                      g                               ��     �  �Y                                     �Y     
                $Z       
       
       t[  @        
 4[              � ߱        P�  V   ?  \�  ���                        �[     
                \       
       
       T]  @        
 ]              � ߱        |�  V   f  �  ���                         �    �  ��  ��      h]      4   ����h]      $   �  Ա  ���                       �]  @         �]              � ߱        Գ  g   �  �         ��x�        �]  ��x�        �]                  ��          Ĳ  ��      ��                  �  �  ܲ              ��                    O   ����    e�          O   ����    R�          O   ����    ��            �  �   �      �]      4   �����]      O  �  ������  ^    ��                            ����                                        @�              =      8�                      g                               ��  g   �  �         �6$�         ^                  ��          ��  l�      ��                  �  �  ��              �                    O   ����    e�          O   ����    R�          O   ����    ��      ̴    �  (^  }          O  �  ������  <^    ��                            ����                                         �              >      �                      g                               и  g   �  ��         �"t�                           ��          0�  �      ��                 �  �  H�              D                    O   ����    e�          O   ����    R�          O   ����    ��                                                       � ߱        Զ  $   �  `�   �                       ط    �  �  l�      P^      4   ����P^                |�                      ��                  �  �                  �                       �   �  ��  	  �  ��                                        3   �����^      O  �  ������  �^      /   �  �                                 3   �����^    ��                              ��        7                  ����                                        ��              ?      �                      g                                     �  �  h�      �^      4   �����^                ܹ                      ��                  �                    X��                       �  ��  �^  @                     _  @         �^          4_  @          _              � ߱        �  $   �  x�  ���                       �  g   �   �         �n��      }                      �          ��  ��      ��                  �  �  к              ���                    O   ����    e�          O   ����    R�          O   ����    ��      $�  /  �  �                                 3   ����@_        �  @�  P�      \_      4   ����\_      O  �  ������  �_    ��                            ����                                        4�              @      h�                      g                               ؽ  g   �  �         �!|�         �_                  �          ��  ��      ��                  �  �  ̼              D��                    O   ����    e�          O   ����    R�          O   ����    ��      �_  @                         � ߱            $  �  �  ���                         ��                            ����                                        0�              A      <�                      g                               �  /   �  �                                 3   �����_          0�  ��      �_      4   �����_                (�                      ��                                      ���                         @�                h�          P�  8�      ��                                     �t�                         ��      O       ��          O       ��      ��  /   	  ��                                 3   �����_        
  ��  п      `      4   ����`      k     �              }       n        �   adm-create-objects  T�  �                      B      �                               �                     disable_UI  �  t�                      C      <                              �  
                   enable_UI   ��  ��                      D      �                              �  	                   exitObject  ��  D�                      E      �                               �  
                   ue-cargar-articulos P�  ��          (         F     d                          `  h                     ue-fecha-proceso    ��  �                      G      �                              �                     ue-gen-excel    0�  ��          |%  �%    H     $&                           &  D&                     ue-grabar   ��  ��          �      "   I     �                          �  �&  	                   ue-procesar �  `�                      J      L                              �&                     ue-verifica-stock   l�  ��          P      $   K     �                          �  W'                      ����   �   ���  �        t�  8   ����&   ��  8   ����&   ��  8   ����%   ��  8   ����%   ��  %  ��  8   ����#   ��  8   ����#   ��  8   ����   ��  8   ����   �    �  8   ����   �  8   ����   ,�    4�  8   ����   D�  8   ����   t�    T�  8   ����   d�  8   ����       8   ����       8   ����             ��  ��      toggleData  ,INPUT plEnabled LOGICAL    |�  ��  ��      showMessageProcedure    ,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL  ��   �  ,�      returnFocus ,INPUT hTarget HANDLE   �  T�  h�      repositionObject    ,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL    D�  ��  ��      removeLink  ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE ��  �  �      removeAllLinks  ,   ��  (�  8�      modifyUserLinks ,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE �  ��  ��      modifyListProperty  ,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER    ��  �  (�      hideObject  ,   �  <�  T�      editInstanceProperties  ,   ,�  h�  x�      displayLinks    ,   X�  ��  ��      createControls  ,   |�  ��  ��      changeCursor    ,INPUT pcCursor CHARACTER   ��  ��  ��      applyEntry  ,INPUT pcField CHARACTER    ��  $�  4�      adjustTabOrder  ,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER �  ��  ��      addMessage  ,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER |�  ��  ��      addLink ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE ��  L�  \�      unbindServer    ,INPUT pcMode CHARACTER <�  ��  ��      startServerObject   ,   t�  ��  ��      runServerObject ,INPUT phAppService HANDLE  ��  ��  ��      restartServerObject ,   ��  �  (�      initializeServerObject  ,    �  <�  P�      disconnectObject    ,   ,�  d�  x�      destroyServerObject ,   T�  ��  ��      bindServer  ,   |�  ��  ��      processAction   ,INPUT pcAction CHARACTER   ��  ��  ��      enableObject    ,   ��  �  �      disableObject   ,   ��  0�  <�      applyLayout ,    �  P�  \�      viewPage    ,INPUT piPageNum INTEGER    @�  ��  ��      viewObject  ,   x�  ��  ��      toolbar ,INPUT pcValue CHARACTER    ��  ��  ��      selectPage  ,INPUT piPageNum INTEGER    ��  �  (�      removePageNTarget   ,INPUT phTarget HANDLE,INPUT piPage INTEGER �  d�  p�      passThrough ,INPUT pcLinkName CHARACTER,INPUT pcArgument CHARACTER  T�  ��  ��      notifyPage  ,INPUT pcProc CHARACTER ��  ��  ��      initPages   ,INPUT pcPageList CHARACTER ��  $�  @�      initializeVisualContainer   ,   �  T�  h�      initializeObject    ,   D�  |�  ��      hidePage    ,INPUT piPageNum INTEGER    l�  ��  ��      destroyObject   ,   ��  ��  ��      deletePage  ,INPUT piPageNum INTEGER    ��  �   �      createObjects   ,    �  4�  D�      constructObject ,INPUT pcProcName CHARACTER,INPUT phParent HANDLE,INPUT pcPropList CHARACTER,OUTPUT phObject HANDLE $�  ��  ��      confirmExit ,INPUT-OUTPUT plCancel LOGICAL  ��  ��   �      changePage  ,   ��  �  (�      assignPageProperty  ,INPUT pcProp CHARACTER,INPUT pcValue CHARACTER      � 
"     
 %     adecomm/as-utils.w 
"   
   �    }        �
"     
       �     }        �G� k   �G%              � o     %              %       P       %        %       %        %       %               %               %               %              %              %              %               %              
�        
"   
 
�    
"   
 
"   
 �    �        �     �        �    
"   
   �        0         �     }        �%              
"   
 
"   
 �    �        �     �        �    
"   
   �        �         �     }        �%              � 
" 
   
 �%              � �  �         X      $              
�    � �   �     
"   
 �                      
�            � �   �
" 
   
 �
�H T   %              �     }        �GG %              � 
"   
   P �L 
�H T   %              �     }        �GG %              
"   
   �        �    7%               
"   
 ��           �    1� �  
 �� �   �%               o%   o           � �    �
"   
 ��           0    1� �   �� �   �%               o%   o           � �   �
"   
 ��           �    1� �  
 �� �   �%               o%   o           � �   �
"   
 ��               1� �   �� �   �%               o%   o           �    �
"   
 ��           �    1�    �� �   �%               o%   o           �    �
"   
 ��                1� 1   �� =   �%               o%   o           %               
"   
 ��          |    1� E   �� U     
"   
 ��           �    1� \   �� �   �%               o%   o           � o  e �
"   
 ��           ,    1� �   �� �   �%               o%   o           � �  [ �
"   
 ��           �    1� @   �� =   �%               o%   o           %               
"   
 ��           	    1� P   �� =   �%               o%   o           %               
"   
 ��           �	    1� b   �� =   �%               o%   o           %              
"   
 ��          
    1� o   �� =     
"   
 ��           P
    1� ~  
 �� =   �%               o%   o           %               
"   
 ��           �
    1� �   �� �   �%               o%   o           � �    �
"   
 ��          @    1� �   �� U     
"   
 ��           |    1� �   �� �   �%               o%   o           � �  t �
"   
 ��          �    1� ,  
 �� U     
"   
 ��           ,    1� 7   �� �   �%               o%   o           � H  � �
"   
 ��           �    1� �   �� �   �%               o%   o           � �    �
"   
 ��               1� �  
 �� �   �%               o%   o           %               
"   
 ��           �    1� �   �� =   �%               o%   o           %               
"   
 �               1�    � �   �%               o%   o           � �    �
"   
 �           �    1�    � �   �%               o%   o           o%   o           
"   
 ��           �    1� $  
 �� �   �%               o%   o           � �    �
"   
 �           p    1� /   � @  	 �%               o%   o           � J  / �
"   
 ��          �    1� z   �� @  	   
"   
 ��                1� �   �� @  	 �o%   o           o%   o           � �    �
"   
 ��          �    1� �   �� @  	   
"   
 ��           �    1� �   �� @  	 �o%   o           o%   o           � �    �
"   
 ��          D    1� �   �� =     
"   
 ��          �    1� �   �� @  	   
"   
 ��          �    1� �   �� @  	   
"   
 ��          �    1� �   �� @  	   
"   
 ��           4    1� �   �� =   �o%   o           o%   o           %              
"   
 ��          �    1�    �� @  	   
"   
 ��          �    1�   
 ��      
"   
 ��          (    1� &   �� @  	   
"   
 ��          d    1� 5   �� @  	   
"   
 ��          �    1� H   �� @  	   
"   
 ��          �    1� ]   �� @  	   
"   
 ��              1� l  	 �� @  	   
"   
 ��          T    1� v   �� @  	   
"   
 ��          �    1� �   �� @  	   
"   
 �           �    1� �   � �   �%               o%   o           o%   o           
�H T   %              �     }        �GG %              
"   
   
"   
 �
"   
   
"   
 �(�  L ( l       �        �    �� �   � P   �        �    �@    
� @  , 
�       �    �� �     p�               �L
�    %              � 8      �    � $         � �          
�    � �     
"   
 �� @  , 
�       �    �� �  
 �p�               �L"      P �L 
�H T   %              �     }        �GG %              
"   
 ��           t    1� �  
 �� �   �%               o%   o           � �    �
"   
 ��           �    1� �  
 �� �   �%               o%   o           o%   o           
"   
 ��           d    1� �   �� U   �%               o%   o           o%   o           
"   
 �           �    1� �   � =   �%               o%   o           %               
"   
 ��           \    1�    �� =   �%               o%   o           %               
"   
 �           �    1�    � �   �%               o%   o           � �    �
"   
 ��           L    1�    �� =   �%               o%   o           %              
"   
 ��           �    1� -   �� =   �%               o%   o           o%   o           
"   
 ��           D    1� 9   �� �   �%               o%   o           o%   o           
"   
 ��           �    1� G  	 �� �   �%               o%   o           � �    �
"   
 ��           4    1� Q   �� �   �%               o%   o           o%   o           
"   
 ��           �    1� e   �� �   �%               o%   o           o%   o           
"   
 ��           ,    1� t   �� =   �%               o%   o           %               
"   
 ��           �    1� �   �� =   �%               o%   o           o%   o           P �L 
�H T   %              �     }        �GG %              
"   
 ��           x    1� �   �� @  	 �%               o%   o           � �    �
"   
 ��           �    1� �   �� @  	 �%               o%   o           � �    �
"   
 ��           `    1� �   �� =   �%               o%   o           %               
"   
 �           �    1� �   � @  	 �%               o%   o           � �    �
"   
 ��           P     1� �   �� @  	 �%               o%   o           � �    
"   
 ��           �     1� �   �� =   �%               o%   o           %               
"   
 �           @!    1� �   � @  	 �%               o%   o           � �    �
"   
 ��           �!    1� �   �� @  	 �%               o%   o           � �    
"   
 ��           ("    1�    �� @  	 �%               o%   o           � �    �
"   
 ��           �"    1�    �� @  	 �%               o%   o           o%   o           
"   
 ��           #    1�    �� @  	 �%               o%   o           � �    �
"   
 �           �#    1� .   � @  	 �%               o%   o           � �    �
"   
 ��            $    1� <  	 ��    �%               o%   o           %               
"   
 ��           |$    1� F   ��    �%               o%   o           %               
"   
 ��           �$    1� O   �� =   �%               o%   o           o%   o           
"   
 �           t%    1� `   � =   �%               o%   o           o%   o           
"   
 ��           �%    1� o   �� =   �%               o%   o           %               
"   
 ��           l&    1� }   �� =   �%               o%   o           %               
"   
 ��           �&    1� �   �� =   �%               o%   o           %               
"   
 �           d'    1� �   � �   �%               o%   o           %       
       
"   
 �           �'    1� �   � �   �%               o%   o           o%   o           
"   
 ��           \(    1� �   �� �   �%               o%   o           %              
"   
 ��           �(    1� �   �� �   �%               o%   o           o%   o           
"   
 ��           T)    1� �   �� �   �%               o%   o           %              
"   
 ��           �)    1� �   �� �   �%               o%   o           o%   o           
"   
 ��           L*    1� �   �� �   �%               o%   o           %              
"   
 ��           �*    1� �   �� �   �%               o%   o           o%   o           
"   
 �           D+    1�    � @  	 �%               o%   o           � �    �P �L 
�H T   %              �     }        �GG %              
"   
 ��           ,    1�    �� �   �%               o%   o           %               
"   
 ��           �,    1� #   �� �   �%               o%   o           o%   o           
"   
 ��           -    1� /   �� �   �%               o%   o           � �    �
"   
 ��           x-    1� ?   �� �   �%               o%   o           � U  - �
"   
 ��           �-    1� �   �� �   �%               o%   o           � �    �
"   
 ��           `.    1� �   �� �   �%               o%   o           � �   �
"   
 ��          �.    1� �   �� U     
"   
 ��           /    1� �   �� �   �%               o%   o           � �    �
"   
 ��          �/    1� �  
 �� U     
"   
 ��          �/    1� �   �� U     
"   
 ��           �/    1� 
   �� @  	 �%               o%   o           � �    �
"   
 ��           p0    1�    �� �   �%               o%   o           � �    �
"   
 ��           �0    1� $   �� U   �%               o%   o           o%   o           
"   
 ��           `1    1� 1   �� �   �%               o%   o           � D  ! 
"   
 ��           �1    1� f   �� �   �%               o%   o           � �    �
"   
 �           H2    1� s   � �   �%               o%   o           � �   �
"   
 �           �2    1� �  	 � �   �%               o%   o           o%   o           
"   
 ��           83    1� �   �� =   �%               o%   o           %               
"   
 ��          �3    1� �   �� U     
"   
 ��           �3    1� �   �� �   �%               o%   o           � �   �
"   
 �           d4    1� �   � @  	 �%               o%   o           � �    �
"   
 ��           �4    1� �   �� @  	 �%               o%   o           � �    
"   
 ��          L5    1� �   �� U     
"   
 ��          �5    1�    �� @  	   
"   
 �           �5    1�    � =   �o%   o           o%   o           %               
"   
 ��          @6    1� 5   �� =     
"   
 ��          |6    1� L   �� @  	   
"   
 ��          �6    1� Z   �� @  	   
"   
 ��          �6    1� m   �� @  	   
"   
 ��          07    1� ~   �� @  	   
"   
 ��          l7    1� �   �� @  	   
"   
 ��          �7    1� �   �� U     
"   
 ��           �7    1� �   �� �   �%               o%   o           � �  4 �
"   
 ��          X8    1� �   �� U     
"   
 ��          �8    1� 
   �� U     
"   
 ��          �8    1�    �� U     
"   
 ��          9    1� '   �� @  	   
"   
 ��          H9    1� ;   �� @  	   
"   
 ��          �9    1� M   �� @  	   
"   
 ��          �9    1� _   �� =     
"   
 ��           �9    1� l   �� @  	 �%               o%   o           � �    �
"   
 ��           p:    1� z   �� @  	 �%               o%   o           � �    �
"   
 ��           �:    1� �   �� @  	 �%               o%   o           � �    �
"   
 ��           X;    1� �   �� @  	 �%               o%   o           � �    �
"   
 ��           �;    1� �   �� =   �%               o%   o           %               
"   
 ��           H<    1� �   �� =   �%               o%   o           o%   o           
"   
 �           �<    1� �   � =   �%               o%   o           %               
"   
 ��           @=    1� �   �� =   �%               o%   o           %               
"   
 ��           �=    1� �   �� =   �%               o%   o           o%   o           
"   
 ��           8>    1�    �� =   �%               o%   o           %               
"   
 ��          �>    1�    �� @  	   
"   
 ��           �>    1� #   �� =   �%               o%   o           %              
"   
 ��          l?    1� 4   �� @  	   
"   
 ��          �?    1� @   �� @  	   
"   
 ��          �?    1� O  
 �� @  	   
"   
 ��            @    1� Z   �� @  	 �%               o%   o           � �   �
"   
 ��           �@    1� l   �� @  	 �%               o%   o           � �    �
"   
    "  	  �%     start-super-proc ��%     adm2/smart.p ��P �L 
�H T   %              �     }        �GG %              
"   
   �       �A    6� �     
"   
   
�        �A    8
"   
   �         B    ��     }        �G 4              
"   
 ߱G %              G %              %p e `   LogicalObjectName,PhysicalObjectName,DynamicObject,RunAttribute,HideOnInit,DisableOnInit,ObjectLayout �
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        HC    �� �   � P   �        TC    �@    
� @  , 
�       `C    �� �   �p�               �L
�    %              � 8      lC    � $         � �          
�    � �   �
"   
 �p� @  , 
�       |D    �� \   �p�               �L"    , �   � �   �� �   ��     }        �A      |    "      � �   �%              (<   \ (    |    �     }        �A� �   �A"    �    "    �"    �  < "    �"    �(    |    �     }        �A� �   �A"    �
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        PF    �� �   � P   �        \F    �@    
� @  , 
�       hF    �� �   �p�               �L
�    %              � 8      tF    � $         � �          
�    � �   �
"   
 �p� @  , 
�       �G    �� �  
 �p�               �L"    , 
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        (H    �� �   � P   �        4H    �@    
� @  , 
�       @H    �� �   �p�               �L
�    %              � 8      LH    � $         � �          
�    � �   �
"   
 �p� @  , 
�       \I    �� E   �p�               �L
"   
 , 
�H T   %              �     }        �GG %              
"   
   
"   
 �
"   
   
"   
   (�  L ( l       �         J    �� �   � P   �        J    �@    
� @  , 
�       J    �� �     p�               �L
�    %              � 8      $J    � $         � �          
�    � �     
"   
 �p� @  , 
�       4K    �� �  
 �p�               �L%     SmartWindow 
"   
   p� @  , 
�       �K    �� �     p�               �L%      WINDOW  
"   
  p� @  , 
�       �K    �� �    p�               �L%               
"   
  p� @  , 
�       XL    �� �    p�               �L(        � �      � �      � �      �     }        �A
�H T   %              �     }        �GG %              
"   
 � (   � 
"   
 �    �        8M    �� �   �
"   
   � 8      �M    � $         � �          
�    � �   �
"   
   �        �M    �
"   
   �       �M    /
"   
   
"   
   �       (N    6� �     
"   
   
�        TN    8
"   
   �        tN    �
"   
   �       �N    �
"   
   p�    � �   �
�    �     }        �G 4              
"   
 ߱G %              G %              
�     }        �
"   
    (   � 
"   
 �    �        XO    �A"    �A
"   
   
�        �O    �@ � 
"   
 �"      �       }        �
"   
 �%              %                "  	  �%     start-super-proc ��%     adm2/appserver.p ���    � W     
�    �     }        �%               %      Server  - �     }        �    "    �� �    �%                   "    �� �    �%      NONE    p�,  8         $     "            � q   �
�    
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        �Q    �� �   � P   �        �Q    �@    
� @  , 
�       �Q    �� �   �p�               �L
�    %              � 8      R    � $         � �          
�    � �   �
"   
 �p� @  , 
�       S    �� Q   �p�               �L"    , p�,  8         $     "            �    �
�     "  	  �%     start-super-proc ��%     adm2/visual.p ��   � �     � �     � �  )   
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        tT    �� �   � P   �        �T    �@    
� @  , 
�       �T    �� �   �p�               �L
�    %              � 8      �T    � $         � �          
�    � �   �
"   
 �p� @  , 
�       �U    �� �   �p�               �L"    , � 
"    
 �%     contextHelp 
"    
   
�    
�    %     processAction   
�    %     CTRL-PAGE-UP ��%     processAction   
�    %     CTRL-PAGE-DOWN  "  	  �%     start-super-proc �%     adm2/containr.p %     modifyListProperty 
�    
�    %      Add     %      ContainerSourceEvents �%      initializeDataObjects �0 0   A    �    �    �
�    � '   �A    �    �      
�    � 3   �%     modifyListProperty 
�    
�    %      Add     %      ContainerSourceEvents �%     buildDataRequest ent0 A    �    �    �
�    � P   �%     modifyListProperty 
�    
�    %      Add     %     SupportedLinks %      ContainerToolbar-Target � 
"    
 �
"   
 �%     contextHelp 
"    
   
�    
�    %               
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
 �(�  L ( l       �        �Y    �� �   � P   �         Z    �@    
� @  , 
�       Z    �� �   �p�               �L
�    %              � 8      Z    � $         � �   �     
�    � �   �
"   
 �p� @  , 
�       ([    �� �   �p�               �L
�             �G
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
 �(�  L ( l       �        �[    �� �   � P   �        �[    �@    
� @  , 
�       �[    �� �   �p�               �L
�    %              � 8      �[    � $         � �   �     
�    � �   �
"   
 �p� @  , 
�       ]    �� �   �p�               �L%              (        �     }        �G� k   �G� 
"   
 �
"   
   �        �]    �%              
"   
 �
"   
 ��     }        �%               
"   
 �%      CLOSE   %                         "    �� �    �    "    �� �    �� �  7   %               %     ue-procesar � 
"   
 �
"   
 �
"   
 ��        �^    %%              
�     }        �
"   
   %     destroyObject       �     }        �    �  � �  	   %               
"   
 �
�    %     createObjects    �     }        �%     initializeObject � �     }        �(        �     }        �G� k   �G� 
"   
 �
"   
   �     }        �
�    
"   
 �"    �"    �"      
"   
 �
"   
   %      CLOSE   %               � /   �� 7   �� >   �"     �&    &    \    8        %                  "      &        "      &        "      &    "    �    "      &     *        %              %                   "      %                  "      %              %              %              "      � �      "      "     �"    �&    &    &    &        %              %              "      (   *    "  I    � f     %               $ $   4         %              4         %              %               $ $   4         %              4         %              %              "  4    %              +  � �   �&    &    *    "      � �      %              � J      "      �d                              � \      p�  �d  �d         %               "      @e                              � d      p�  e   e         %                   "    �%              "      �e                              �e                             � s   	   � �     �  �e  �e        "      `f                              �f        lf  pf  tf          � s   	   xf         "      � }      �  f   f        "      �f                              g        �f   g  g          � �      g         %              � �      �  �f  �f        %               %              ' (             �    �    �    �    �    �    t    `    L    8    $        �     �     �     �     �     �     �     p     \     H     4               � �   3 �� �   ' �� �   ' �� !  ' �� :!  ' �� b!  ' �� �!  ' �� �!  ' �� �!  ' �� "  ' �� *"  ' �� R"  ' �� z"  '   � �"  ' �� �"  ' �� �"  ' �� #  '   � B#  ' �� j#  '   � �#  ' �� �#  '   � �#  ' �� 
$  '  � 2$  ' �� Z$  '   � �$  '   � �$  '   � �$  '   � �$  '   %                   "           � "%     "      "      Lj        4j  8j  <j          Xj                              @j         "      � $%     � *%     p�  �i  �i         � 0%          � 7%     "      "      k        �j  �j  �j          k                              �j         "      � $%     � *%     p�  �j  �j         � 9%          � E%     "      "      �k        �k  �k  �k          �k                              �k         "      � $%     � *%     p�  `k  lk         � G%          � P%     "      "      �l        hl  ll  pl          �l                              tl         "      � $%     � *%     p�  l  (l         � R%          � d%     "      "      <m        $m  (m  ,m          Hm                              0m         "      � $%     � *%     p�  �l  �l         � f%          � s%     "      "      �m        �m  �m  �m          n                              �m         "      � $%     � *%     p�  �m  �m         � u%          � �%     "      "      �n        �n  �n  �n          �n                              �n         "      � $%     � *%     p�  Pn  \n         � �%          � �%     "      "      po        Xo  \o  `o          |o                              do         "      � $%     � *%     p�  o  o         � �%          � �%     "      "      ,p        p  p  p          8p                               p         "      � $%     � *%     p�  �o  �o         � �%          � �%     "      "      �p        �p  �p  �p          �p                              �p         "      � $%     � *%     p�  �p  �p         � �%          � �%     "      "      �q        �q  �q  �q          �q                              �q         "      � $%     � *%     p�  @q  Lq         � �%          � �%     "      "      `r        Hr  Lr  Pr          lr                              Tr         "      � $%     � *%     p�  �q  r         � �%          � �%     "      "      s        s  s  s          (s                              s         "      � $%     � *%     p�  �r  �r         � �%     %              "     "    &    &    &    &        %              %              � &   �"    �&    &    &    &        %              %              *    "  4        "  
    %                  "      "  F         "      %                   "           � "%     "      "      Tu        <u  @u  Du          `u                              Hu         "      � $%     � *%     p�  �t  �t              � &     "           � 7%     "      "      $v        v  v  v          0v                              v         "      � $%     � *%     p�  �u  �u           (   � &     (   *    "      � �           � E%     "      "      w        �v  �v   w          w                              w         "      � $%     � *%     p�  �v  �v              � &     "           � P%     "      "      �w        �w  �w  �w          �w                              �w         "      � $%     � *%     p�  |w  �w           (   � &     (   *    "      � �      $    4         %              � &          � d%     "      "      y        �x  �x  �x          y                              �x         "      � $%     � *%     p�  �x  �x          P $    $ $   4        %              4         %       	       4         %       
            � s%     "      "      0z        z  z   z          <z                              $z         "      � $%     � *%     p�  �y  �y          P $    $ $   4        %              4         %       	       4         %       
            � �%     "      "      \{        D{  H{  L{          h{                              P{         "      � $%     � *%     p�  �z  {         |     P $    $ $   4         %              4         %       	       4         %       
       "           � �%     "      "      �|        �|  �|  �|          �|                              �|         "      � $%     � *%     p�  8|  D|         4         %                   � �%     "      "      p}        X}  \}  `}          |}                              d}         "      � $%     � *%     p�  }  }         $    4        %              "           � �%     "      "      X~        @~  D~  H~          d~                              L~         "      � $%     � *%     p�  �}   ~         4         %       	            � �%     "      "      ,                      8                                        "      � $%     � *%     p�  �~  �~         $    4        %       	       "           � �%     "      "      �        �   �  �           �                              �         "      � $%     � *%     p�  �  �         4         %       
            � �%     "      "      �        Ѐ  Ԁ  ؀          �                              ܀         "      � $%     � *%     p�  ��  ��         $    4        %       
       "      "      x�                              � d      p�  L�  X�         %                  "    �%                  "    �� �    �"       �                              � &     p�  �   �         %               "    ���        ��  ��  ��          ��         "    �� &&   �p�  T�  `�             "    �%               "      �                              � \      p�  ��  �         %              "      l�                              � \      p�  @�  L�         %               "    �̃                             � -&   �p�  ��  ��         "      "      "      "          "    �%              � 2&     � �&   �"     �&    &    &    &        %              %              � #   " "     &        � !   &    * !   � �&     $    4         %              &    � �&     "       �    }        �� �&     %     ue-cargar-articulos %     ue-fecha-proceso �%     ue-verifica-stock �% 	    ue-grabar �%     ue-gen-excel    �    }        �� �      � �&     %               � �      � �      � �      � �      %               $ $   4         %              4         %                  " $   �%               "     "    "    &    &    &    &    &    &    0        %              %              %              (   * %   " %     %               " $     %              "      %                   " $     " $     %                   %              %                   " $     %                  " $     �     "      �     "      T   " $     "      � <'     "     " $   "    &    &    &    &    &    &    0        %              %              %              (   * %   " %     %                   " $   �%                    " $     " $      T      @   " $     (        " $   �� �    �� �    �� <'   �" $   � T       @   " $ 	    (        " $ 	  �� �    �� �    �� <'   �    " $   �� >'   �     " $     " $     " $     %              " $     %              " $ 	    %                  " $     " $         " $   �%               %               � T'     "       &    &     8   %              (    -$ 4    &     %              &    " &       <       " $     "    �     S    " $     "    �%               "     " $   "    &    &    &    &    &    &    0        %              %              %              (   * %   " %     %                   " $   �%                T      @   " $ 
    (        " $ 
  �� �    �� �    �� <'   �" $   � T       @   " $     (        " $   �� �    �� �    �� <'   �    " $   �� >'   �     " $     " $          " $     " $     " $     %              " $ 
    %              " $     %       	       $    4         %              � &     (        " $     " $     " $     " $     %              (         " $    " $         " $     " $     %               %       	       (        " $     " $     " $     " $     %              (         " $    " $         " $     " $     %               %       
       � �&     %                              �           �   l       ��                 0  T  �               (��                    O   ����    e�          O   ����    R�          O   ����    ��        $  ?  �   ���                       �L     
                    � ߱              @  (  �      DM      4   ����DM                �                      ��                  A  S                  q�                       A  8  �  �  B  �M            D  �  `      �M      4   �����M                p                      ��                  E  R                  �q�                       E  �  �  o   F      ,                                 �  �   G  N      �  �   H  4N      $  $  I  �  ���                       `N     
                    � ߱        8  �   J  �N      L  �   K  �N      `  �   N  �N          $   Q  �  ���                       �N  @         �N              � ߱                     T          ,  @   T �            
             
             
             
                 $   4   D          $   4   D   ����        ��                            ����                                            �           �   l       ��                 x  �  �               $��                    O   ����    e�          O   ����    R�          O   ����    ��      �                      �          �  $  �    ���                       DO     
                    � ߱                  �  �                      ��                   �  �                  t\�                     �  4      4   ����dO      $  �  �  ���                       �O     
                    � ߱        �    �  4  D      �O      4   �����O      /  �  p                               3   �����O  �  �   �  �O          O   �  ��  ��  P                               , �                          
                               �      ��                            ����                                                        �   l       ��                    "  �               hu�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                                            �           �   l       ��                  (  5  �               ��                    O   ����    e�          O   ����    R�          O   ����    ��           2  �   �       $`      4   ����$`      n   3     �          d`        4    ,      p`      4   ����p`      �   4  �`    ��                            ����                                            �           �   l       ��                  ;  K  �               ط�                    O   ����    e�          O   ����    R�          O   ����    ��      �`  �           �`  �          �`  �              � ߱        p  Z   E  �    �        �`                  �               �              �              �              � ߱        �  h   G      �        �`                  
   J  �� �             �`    ��                              ��        7                  ����                                            �           �   l       ��                  Q  [  �               ��                    O   ����    e�          O   ����    R�          O   ����    ��      �     X  �`  }          O   Y  ��  ��  �`    ��                            ����                                            �           �   l       ���               a  �  �               ��                    O   ����    e�          O   ����    R�          O   ����    ��      0  �   i           @      �  �      �  �      ��                  k  �  �              ੔                       k  �       l  �       ��                            7   ����          ��               <a    �                              6   k        P   ��         0  <a    �                                                                    �`   a   a    a                   �  �           ,a           4a                      l   |        O   ����  e�          O   ����  R�          O   ����  ��      �  A  n        P   ��         D  �a                                         �a                 �  �                                   @            l   |    �    p  �  D      �a      4   �����a                T                      ��                  p                    ��                       p  �  �  9   r       �      8  0          �  �      ��       0         u  y  �               8�      pb     \     u  d      $  u    ���                       �a                         � ߱        �  $  u  d  ���                        b                         � ߱            4   ����Hb      O   ����  e�          O   ����  R�          O   ����  ��      �b         �b         �b          �b             � ߱            $  v  �  ���                       l  A  |        �   ��         �  �b                                         �b   �b                                �b  �b           �b  �b         �            �   �    $c                     0c          Xc             � ߱            $  }  $  ���                       lc         �c         �c          d         4d         @d             � ߱            $  �  �  ���                                     \                                           ��                             ��                             ��                            ����                            �        =   �                           �           �   l       ��               �  �  �               ��                    O   ����    e�          O   ����    R�          O   ����    ��        $  �  �   ���                       Td                         � ߱        �  A  �        h   ��         \                                             Xd                 �  �           dd           ld         �            �   �          �  �  \      td      4   ����td                l                      ��                  �  �                  �l�                       �  �      $  �  �  ���                       |d                         � ߱          ��                            ����                                                  �           �   l       ���&               �  �  �               Hm�                    O   ����    e�          O   ����    R�          O   ����    ��        $  �  �   ���                       �d                         � ߱        d  $  �  8  ���                       �d                         � ߱        �  o   �           �d                          �  $   �  �  ���                        e  @        	 �d              � ߱        H  $   �    ���                       `e  @        	 Le              � ߱            �  d  �  �  te      4   ����te                �                      ��                  �  �                  ��                       �  t      $  �    ���                        f                         � ߱                      �                      ��                  �  �                  �s�                       �  H      $  �  �  ���                       �f                         � ߱        t  $  �  H  ���                       (g                         � ߱        �  $  �  �  ���                       <g                         � ߱        P  $  �  �  ���                       Pg                         � ߱        dg     '                    � ߱        |  V   �  $  ���                        �  $     �  ���                       �i                         � ߱        ,  $       ���                       �i                         � ߱        �  $    X  ���                       �i                         � ߱        �  $     �  ���                       xj  @        	 dj              � ߱        4  $      ���                       �j                         � ߱        �  $     `  ���                       4k  @        	  k              � ߱        �  $    �  ���                       @k                         � ߱        <  $       ���                       �k  @        	 �k              � ߱        �  $  	  h  ���                       �k                         � ߱        �  $   
  �  ���                       �l  @        	 �l              � ߱        D	  $    	  ���                       �l                         � ߱        �	  $     p	  ���                       hm  @        	 Tm              � ߱        �	  $    �	  ���                       tm                         � ߱        L
  $      
  ���                       $n  @        	 n              � ߱        �
  $    x
  ���                       0n                         � ߱        �
  $     �
  ���                       �n  @        	 �n              � ߱        T  $    (  ���                       �n                         � ߱        �  $     �  ���                       �o  @        	 �o              � ߱          $    �  ���                       �o                         � ߱        \  $     0  ���                       Xp  @        	 Dp              � ߱        �  $    �  ���                       dp                         � ߱          $     �  ���                       q  @        	  q              � ߱        d  $    8  ���                        q                         � ߱        �  $     �  ���                       �q  @        	 �q              � ߱          $    �  ���                       �q                         � ߱        l  $     @  ���                       �r  @        	 xr              � ߱        �  $    �  ���                       �r                         � ߱        �  $     �  ���                       Hs  @        	 4s              � ߱              �      ,          �  �      ��                    Z                䫔                �             �         ��                            7   ����         ��                     �            d                  6          �   ��                    �            d                                                                �  �                                   @            �   �        O   ����  e�          O   ����  R�          O   ����  ��      �  $  !  X  ���                       Ts                         � ߱        L  A  "        �   ��         �  �s                                         hs   ts                   8  ,           �s  �s           �s  �s         �                     A  $        �   ��         �  t                                         �s   �s                      �           �s  �s           �s   t         �            �   �        '  0  �      8t      4   ����8t                �                      ��                  '  -                  ��                       '  @    $  (  �  ���                       @t                         � ߱              )  0  �      Lt      4   ����Lt                �                      ��                  )  ,                  �                       )  @      $  +  �  ���                       tt                         � ߱        l  $  /  @  ���                       �t                         � ߱        �  $  0  �  ���                       �t                         � ߱          $  2  �  ���                       �t                         � ߱        t  $   3  H  ���                       �u  @        	 lu              � ߱        �  $  4  �  ���                       �u                         � ߱        $  $   5  �  ���                       Pv  @        	 <v              � ߱        |  $  6  P  ���                       �v                         � ߱        �  $   7  �  ���                       <w  @        	 (w              � ߱        ,  $  8     ���                       \w                         � ߱        �  $   9  X  ���                       x  @        	 �w              � ߱            :  �    H  Hx      4   ����Hx                ,                      ��                  :  >                  P��                       :  �  �  $  <  X  ���                       �x                         � ߱            $   =  �  ���                       0y  @        	 y              � ߱                      X                      ��                  ?  C                  @��                       ?  �  �  $  A  �  ���                       �y                         � ߱            $   B  �  ���                       \z  @        	 Hz              � ߱        `  $  E  4  ���                       �z                         � ߱        �  $   F  �  ���                       �{  @        	 t{              � ߱          $  I  �  ���                       |                         � ߱        h  $   J  <  ���                       �|  @        	 �|              � ߱        �  $  K  �  ���                       �|                         � ߱          $   L  �  ���                       �}  @        	 �}              � ߱        p  $  O  D  ���                       �}                         � ߱        �  $   P  �  ���                       �~  @        	 p~              � ߱           $  Q  �  ���                       �~                         � ߱        x  $   R  L  ���                       X  @        	 D              � ߱        �  $  U  �  ���                       �                         � ߱        (  $   V  �  ���                       @�  @        	 ,�              � ߱        �  $  W  T  ���                       d�                         � ߱            $   X  �  ���                       �  @        	  �              � ߱        0   $   ^     ���                       ��  @        	 ��              � ߱        h"    `  L   �   X"  ��      4   ������                �                       ��             	     `  f                  x�                       `  \         b  �   p!      ԁ      4   ����ԁ  	              �!                      ��             	     b  e                  ��                       b  !  �!  $   c  �!  ���                       @�  @        	 ,�              � ߱            �   d  ��      
                                      ��             
     g  l                  ��                       g  �!  P$    m  �"   #  �#  ��      4   ������                #                      ��                  m  o                  �                       m  �"      $   n  <#  ���                       ,�  @        	 �              � ߱                      �#                      ��                  p  s                  ��                       p  h#  <$  $   q  $  ���                       ��  @        	 x�              � ߱            �   r  ؃      `$  �  w  �  p$  �  x  ��  �$  �  y  �  �$  �  z  �        |  �$  (%      �      4   �����                8%                      ��                  |  ~                  4�                       |  �$      	  }  l%                                        3   ����D�                �%                                               &          &  &    �%          �      ����       '                        �      ��                             ��                            ����                            �&                          �           �   l       ��                 �  �  �               L�                    O   ����    e�          O   ����    R�          O   ����    ��      0  C   �            @            T  �  �  d  ��                 �  �  �              ��s                �     �  �       l  �       ��                            7   ����   #      ��               ��    �                              6   �       # D   ��         0  ��    �                                                                    P�   \�                   �  �           h�  x�           p�  ��                      `   t        O   ����  e�          O   ����  R�          O   ����  ��      ��      "                   � ߱        H  $  �  �  ���                       �  <  �      !     ����   ̄    	 �  Ԅ                                        ��        �  �  <      �      4   �����                L                      ��                  �  �                  ��s                       �  �      :   �          !         �      �  D      P  8      ��                 �  �  h              \�s                       �  d        \       ��                            7   ����        ��          
      �    �            �                  6   �       �   ��        
 �   �    �            �                                                        �                 $                                     @            �           O   ����  e�          O   ����  R�          O   ����  ��      �  9   �  #   X  �   �   #     �                         	 	 	                                          4�      #               @�      #                   � ߱            V   �    ���                                    "  �                                   �    ! "     ��                             ��                             ��                            ����                                =   �  #       8   �  !       8   �  !                   �           �   l       ��                  �  �  �               4�s                    O   ����    e�          O   ����    R�          O   ����    ��      �   �   �  L�        /   �  �                                  3   ����l�  @  /   �  0                                 3   ������  |  /   �  l                                 3   ������  �  /   �  �                                 3   ����̅  �  /   �  �                                 3   �����    �   �   �          	  �  <                                        3   ���� �    ��                            ����                                            �           �   l       ��               �  :  �               �u                    O   ����    e�          O   ����    R�          O   ����    ��      x  $  �  �   ���                       ,�      $                   � ߱              �                �  �      ��                  �  8                �Pt                       �        �         ��                            7   ����         ��                     �            T                  6   �       x   ��                    �            T                                                                �  �                                   @            �   �        O   ����  e�          O   ����  R�          O   ����  ��      t  $  �  H  ���                       @�      $                   � ߱        �  $  �  �  ���                       L�      $ 	       	           � ߱        $  $  �  �  ���                       X�      $ 
       
           � ߱        |  $  �  P  ���                       d�      $                   � ߱        �  $  �  �  ���                       p�      $                   � ߱        ,  $  �     ���                       ��      $                   � ߱        0    �  H  �      Ԇ      4   ����Ԇ                �                      ��                  �                    H�r                       �  X  �  A  �       % <   ��         $  P�                                        ��   �   �                 �  �            �  0�  @�           (�  8�  H�         �            X   p       $  �  �  ���                       ��      $                   � ߱        X  $  �  ,  ���                       ̇         ؇             � ߱        �  $  �  �  ���                       �          ��             � ߱          $  �  �  ���                       �      $                   � ߱        �  $  �  4  ���                       ,�      $                   � ߱          �      4	  �	                      ��        0         �                     �u    $  ��     (     �  `      $  �  	  ���                       @�      $                   � ߱        �	  $  �  `	  ���                       p�      $                   � ߱            4   ������  �	  $  �  �	  ���                       Ԉ      $                   � ߱        �
  A  �       % \
   ��         D
  T�                                         �   �   �                 �
  �
           $�  4�  D�           ,�  <�  L�         �            x
   �
       $  �  �
  ���                       ��      $                   � ߱              �  <  �      Љ      4   ����Љ                �                      ��                  �                    ��u                       �  L     $  �  �  ���                       ��      $                   � ߱        x  $  �  L  ���                       �      $                   � ߱        �  $  �  �  ���                       ��      $ 	       	           � ߱            $    �  ���                       ��      $                   � ߱        �  $    T  ���                       �         (�             � ߱        �  $    �  ���                       <�          H�             � ߱            $      ���                       \�          h�             � ߱        �  $    \  ���                       |�      $                   � ߱                �     �  ��      4   ������                0                      ��             
       3                  (�u                         �  �  $    \  ���                       ċ      $                   � ߱                    �          |  d      ��                    $  �              ��u                D       �      0  �       ��                            7   ����    &      ��                �    �            �                  6          &    ��         �   �    �            �                                                        ؋   �                   P  D           ��           ��                      $   4        O   ����  e�          O   ����  R�          O   ����  ��        $    �  ���                       T�      $                   � ߱                   �      `�      4   ����`�                �                      ��                    #                  ��t                         0  �  A         %    ��         �  �                                        Č   Ќ   ܌                 l  `           �  ��  �           ��   �  �         �            0   H    �  $    �  ���                       d�      $                   � ߱                �  p      ��      4   ������                �                      ��                    "                  lt                           �  $    �  ���                       ��      $ 
       
           � ߱        0  $      ���                       $�      $                   � ߱        �  $     \  ���                       ��      $                   � ߱            $  !  �  ���                       ��      $                   � ߱        ��         �          �          �          �          ,�             � ߱        p  $  %  �  ���                             )  �    �  @�      4   ����@�  	              `                      ��             	     )  -                  �t                       )  �  x�         ��         ̏         (�             � ߱            $  +    ���                       
              P                      ��             
     .  2                  | t                       .  �  <�         |�         ��         �             � ߱            $  0    ���                                     $                      ��                  4  6                  H!t                       4  |   �          �             � ߱            $  5  �  ���                                   $  �                                              $     ��                             ��                             ��                            ����                                %     L   d d     �   ��@�A  � �                                               7                                                                         d     D                                                                 P   �� ~
d                                                           q'  G   
 X  �� Xd                                                              =      P   �	8�d                                                           �'  G   
 X  �	8|d                                                        *     =      P   �
Eud                                                           �'  G   
 X  �
Exd                                             
                =      \  �?�p                                                  �'                @      P ��>                                                       D        D                                                                    TXS appSrvUtils ASSIGNFOCUSEDWIDGET ASSIGNWIDGETVALUE ASSIGNWIDGETVALUELIST BLANKWIDGET CLEARWIDGET DISABLERADIOBUTTON DISABLEWIDGET ENABLERADIOBUTTON ENABLEWIDGET FORMATTEDWIDGETVALUE FORMATTEDWIDGETVALUELIST HIDEWIDGET HIGHLIGHTWIDGET RESETWIDGETVALUE TOGGLEWIDGET VIEWWIDGET WIDGETHANDLE WIDGETLONGCHARVALUE WIDGETISBLANK WIDGETISFOCUSED WIDGETISMODIFIED WIDGETISTRUE WIDGETVALUE WIDGETVALUELIST s-codcia lFechaProceso tt-factabla Tablas Generales Facturacion CodCia Tabla Codigo Nombre Valor Campo-C Campo-D Campo-L wWin btnProc FILL-IN-1 txtAlmacen txtAlmacenes fMain X(256) Separado por comas (ejm. 11,85,04,etc) GUI Reposicion de Mercaderia DISABLEPAGESINFOLDER ENABLEPAGESINFOLDER GETCALLERPROCEDURE GETCALLERWINDOW GETCONTAINERMODE GETCONTAINERTARGET GETCONTAINERTARGETEVENTS GETCURRENTPAGE GETDISABLEDADDMODETABS GETDYNAMICSDOPROCEDURE GETFILTERSOURCE GETMULTIINSTANCEACTIVATED GETMULTIINSTANCESUPPORTED GETNAVIGATIONSOURCE GETNAVIGATIONSOURCEEVENTS GETNAVIGATIONTARGET GETOUTMESSAGETARGET GETPAGENTARGET GETPAGESOURCE GETPRIMARYSDOTARGET GETREENABLEDATALINKS GETRUNDOOPTIONS GETRUNMULTIPLE GETSAVEDCONTAINERMODE GETSDOFOREIGNFIELDS GETTOPONLY GETUPDATESOURCE GETUPDATETARGET GETWAITFOROBJECT GETWINDOWTITLEVIEWER GETSTATUSAREA PAGENTARGETS SETCALLEROBJECT SETCALLERPROCEDURE SETCALLERWINDOW SETCONTAINERMODE SETCONTAINERTARGET SETCURRENTPAGE SETDISABLEDADDMODETABS SETDYNAMICSDOPROCEDURE SETFILTERSOURCE SETINMESSAGETARGET SETMULTIINSTANCEACTIVATED SETMULTIINSTANCESUPPORTED SETNAVIGATIONSOURCE SETNAVIGATIONSOURCEEVENTS SETNAVIGATIONTARGET SETOUTMESSAGETARGET SETPAGENTARGET SETPAGESOURCE SETPRIMARYSDOTARGET SETREENABLEDATALINKS SETROUTERTARGET SETRUNDOOPTIONS SETRUNMULTIPLE SETSAVEDCONTAINERMODE SETSDOFOREIGNFIELDS SETTOPONLY SETUPDATESOURCE SETUPDATETARGET SETWAITFOROBJECT SETWINDOWTITLEVIEWER GETOBJECTTYPE SETSTATUSAREA GETALLFIELDHANDLES GETALLFIELDNAMES GETCOL GETDEFAULTLAYOUT GETDISABLEONINIT GETENABLEDOBJFLDS GETENABLEDOBJHDLS GETHEIGHT GETHIDEONINIT GETLAYOUTOPTIONS GETLAYOUTVARIABLE GETOBJECTENABLED GETOBJECTLAYOUT GETROW GETWIDTH GETRESIZEHORIZONTAL GETRESIZEVERTICAL SETALLFIELDHANDLES SETALLFIELDNAMES SETDEFAULTLAYOUT SETDISABLEONINIT SETHIDEONINIT SETLAYOUTOPTIONS SETOBJECTLAYOUT SETRESIZEHORIZONTAL SETRESIZEVERTICAL GETOBJECTTRANSLATED GETOBJECTSECURED CREATEUIEVENTS GETAPPSERVICE GETASBOUND GETASDIVISION GETASHANDLE GETASHASSTARTED GETASINFO GETASINITIALIZEONRUN GETASUSEPROMPT GETSERVERFILENAME GETSERVEROPERATINGMODE RUNSERVERPROCEDURE SETAPPSERVICE SETASDIVISION SETASHANDLE SETASINFO SETASINITIALIZEONRUN SETASUSEPROMPT SETSERVERFILENAME SETSERVEROPERATINGMODE gshAstraAppserver gshSessionManager gshRIManager gshSecurityManager gshProfileManager gshRepositoryManager gshTranslationManager gshWebManager gscSessionId gsdSessionObj gshFinManager gshGenManager gshAgnManager gsdTempUniqueID gsdUserObj gsdRenderTypeObj gsdSessionScopeObj ghProp ghADMProps ghADMPropsBuf glADMLoadFromRepos glADMOk ANYMESSAGE ASSIGNLINKPROPERTY FETCHMESSAGES GETCHILDDATAKEY GETCONTAINERHANDLE GETCONTAINERHIDDEN GETCONTAINERSOURCE GETCONTAINERSOURCEEVENTS GETCONTAINERTYPE GETDATALINKSENABLED GETDATASOURCE GETDATASOURCEEVENTS GETDATASOURCENAMES GETDATATARGET GETDATATARGETEVENTS GETDBAWARE GETDESIGNDATAOBJECT GETDYNAMICOBJECT GETINSTANCEPROPERTIES GETLOGICALOBJECTNAME GETLOGICALVERSION GETOBJECTHIDDEN GETOBJECTINITIALIZED GETOBJECTNAME GETOBJECTPAGE GETOBJECTPARENT GETOBJECTVERSION GETOBJECTVERSIONNUMBER GETPARENTDATAKEY GETPASSTHROUGHLINKS GETPHYSICALOBJECTNAME GETPHYSICALVERSION GETPROPERTYDIALOG GETQUERYOBJECT GETRUNATTRIBUTE GETSUPPORTEDLINKS GETTRANSLATABLEPROPERTIES GETUIBMODE GETUSERPROPERTY INSTANCEPROPERTYLIST LINKHANDLES LINKPROPERTY MAPPEDENTRY MESSAGENUMBER PROPERTYTYPE REVIEWMESSAGES SETCHILDDATAKEY SETCONTAINERHIDDEN SETCONTAINERSOURCE SETCONTAINERSOURCEEVENTS SETDATALINKSENABLED SETDATASOURCE SETDATASOURCEEVENTS SETDATASOURCENAMES SETDATATARGET SETDATATARGETEVENTS SETDBAWARE SETDESIGNDATAOBJECT SETDYNAMICOBJECT SETINSTANCEPROPERTIES SETLOGICALOBJECTNAME SETLOGICALVERSION SETOBJECTNAME SETOBJECTPARENT SETOBJECTVERSION SETPARENTDATAKEY SETPASSTHROUGHLINKS SETPHYSICALOBJECTNAME SETPHYSICALVERSION SETRUNATTRIBUTE SETSUPPORTEDLINKS SETTRANSLATABLEPROPERTIES SETUIBMODE SETUSERPROPERTY SHOWMESSAGE SIGNATURE , prepareInstance ObjectName CHAR  ObjectVersion ADM2.2 ObjectType SmartWindow ContainerType WINDOW PropertyDialog adm2/support/visuald.w QueryObject LOGICAL ContainerHandle HANDLE InstanceProperties LogicalObjectName,PhysicalObjectName,DynamicObject,RunAttribute,HideOnInit,DisableOnInit,ObjectLayout SupportedLinks Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source ContainerHidden ObjectInitialized ObjectHidden ObjectsCreated HideOnInit UIBMode ContainerSource ContainerSourceEvents initializeObject,hideObject,viewObject,destroyObject,enableObject,confirmExit,confirmCancel,confirmOk,isUpdateActive DataSource DataSourceEvents dataAvailable,queryPosition,updateState,deleteComplete,fetchDataSet,confirmContinue,confirmCommit,confirmUndo,assignMaxGuess,isUpdatePending TranslatableProperties ObjectPage INT DBAware DesignDataObject DataSourceNames DataTarget DataTargetEvents CHARACTER updateState,rowObjectState,fetchBatch,LinkState LogicalObjectName PhysicalObjectName LogicalVersion PhysicalVersion DynamicObject RunAttribute ChildDataKey ParentDataKey DataLinksEnabled InactiveLinks InstanceId DECIMAL SuperProcedure SuperProcedureMode SuperProcedureHandle LayoutPosition ClassName RenderingProcedure ThinRenderingProcedure Label cType ADMProps Target WHERE Target = WIDGET-H(" ") AppService ASDivision ASHandle ASHasConnected ASHasStarted ASInfo ASInitializeOnRun ASUsePrompt BindSignature BindScope ServerOperatingMode ServerFileName ServerFirstCall NeedContext ObjectLayout LayoutOptions ObjectEnabled LayoutVariable DefaultLayout DisableOnInit EnabledObjFlds EnabledObjHdls FieldSecurity SecuredTokens AllFieldHandles AllFieldNames MinHeight MinWidth ResizeHorizontal ResizeVertical ObjectSecured ObjectTranslated PopupButtonsInFields ColorInfoBG INTEGER ColorInfoFG ColorWarnBG ColorWarnFG ColorErrorBG ColorErrorFG BGColor FGColor FieldPopupMapping CurrentPage PendingPage ContainerTarget ContainerTargetEvents exitObject,okObject,cancelObject,updateActive ContainerToolbarSource ContainerToolbarSourceEvents toolbar,okObject,cancelObject OutMessageTarget PageNTarget PageSource FilterSource UpdateSource UpdateTarget CommitSource CommitSourceEvents commitTransaction,undoTransaction CommitTarget CommitTargetEvents rowObjectState StartPage RunMultiple WaitForObject DynamicSDOProcedure adm2/dyndata.w RunDOOptions InitialPageList WindowFrameHandle Page0LayoutManager MultiInstanceSupported MultiInstanceActivated ContainerMode SavedContainerMode SdoForeignFields NavigationSource NavigationTarget PrimarySdoTarget NavigationSourceEvents fetchFirst,fetchNext,fetchPrev,fetchLast,startFilter CallerWindow CallerProcedure CallerObject DisabledAddModeTabs ReEnableDataLinks WindowTitleViewer UpdateActive InstanceNames ClientNames ContainedDataObjects ContainedAppServices DataContainer HasDbAwareObjects HasDynamicProxy HideOnClose HideChildContainersOnClose HasObjectMenu RequiredPages RemoveMenuOnHide ProcessList PageLayoutInfo PageTokens DataContainerName WidgetIDFileName ghContainer adm2/smart.p cObjectName iStart / \ . hReposBuffer hPropTable hBuffer hTable deleteProperties ADM-CLONE-PROPS pcProcName hProc START-SUPER-PROC cAppService cASDivision cServerOperatingMode adm2/appserver.p getAppService Server NONE setASDivision setAppService cFields adm2/visual.p   txtAlmacen txtAlmacenes FILL-IN-1 btnProc CTRL-PAGE-UP CTRL-PAGE-DOWN adm2/containr.p Add initializeDataObjects getSupportedLinks data-target data-source buildDataRequest containertoolbar-target ContainerToolbar-Target CLOSE  Almacen Principal O Almacenes a Consultar estan ERRADOS iStartPage ADM-ERROR ADM-CREATE-OBJECTS DISABLE_UI ENABLE_UI EXITOBJECT lFiler VtaTabla Tabla general de ventas DISTRIB GLOBAL ORGPROD Almmmatg Cat�logo de Materiales X UE-CARGAR-ARTICULOS RatioCab XXXXX UE-FECHA-PROCESO lFileXls lNuevoFile lCosto chExcelApplication chWorkbook chWorksheet chWorksheetRange iCount iIndex iColumn iRow cColumn cRange lCerrarAlTerminar lMensajeAlTerminar cColList Excel.Application Visible ScreenUpdating Workbooks OPEN Sheets Item A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z ,AA,AB,AC,AD,AE,AF,AG,AH,AI,AJ,AK,AL,AM ,AN,AO,AP,AQ,AR,AS,AT,AU,AV,AW,AX,AY,AZ ,BA,BB,BC,BD,BE,BF,BG,BH,BI,BJ,BK,BL,BM ,BN,BO,BP,BQ,BR,BS,BT,BU,BV,BW,BX,BY,BZ ,CA,CB,CC,CD,CE,CF,CG,CH,CI,CJ,CK,CL,CM ,CN,CO,CP,CQ,CR,CS,CT,CU,CV,CW,CX,CY,CZ ,DA,DB,DC,DD,DE,DF,DG,DH,DI,DJ,DK,DL,DM ,DN,DO,DP,DQ,DR,DS,DT,DU,DV,DW,DX,DY,DZ ,EA,EB,EC,ED,EE,EF,EG,EH,EI,EJ,EK,EL,EM ,EN,EO,EP,EQ,ER,ES,ET,EU,EV,EW,EX,EY,EZ ,FA,FB,FC,FD,FE,FF,FG,FH,FI,FJ,FK,FL,FM ,FN,FO,FP,FQ,FR,FS,FT,FU,FV,FW,FX,FY,FZ ,GA,GB,GC,GD,GE,GF,GG,GH,GI,GJ,GK,GL,GM ,GN,GO,GP,GQ,GR,GS,GT,GU,GV,GW,GX,GY,GZ ,HA,HB,HC,HD,HE,HF,HG,HH,HI,HJ,HK,HL,HM ,HN,HO,HP,HQ,HR,HS,HT,HU,HV,HW,HX,HY,HZ ,IA,IB,IC,ID,IE,IF,IG,IH,II,IJ,IK,IL,IM ,IN,IO,IP,IQ,IR,IS,IT,IU,IV,IW,IX,IY,IZ ,JA,JB,JC,JD,JE,JF,JG,JH,JI,JJ,JK,JL,JM ,JN,JO,JP,JQ,JR,JS,JT,JU,JV,JW,JX,JY,JZ ,KA,KB,KC,KD,KE,KF,KG,KH,KI,KJ,KK,KL,KM ,KN,KO,KP,KQ,KR,KS,KT,KU,KV,KW,KX,KY,KZ ,LA,LB,LC,LD,LE,LF,LG,LH,LI,LJ,LK,LL,LM ,LN,LO,LP,LQ,LR,LS,LT,LU,LV,LW,LX,LY,LZ ,MA,MB,MC,MD,ME,MF,MG,MH,MI,MJ,MK,ML,MM ,MN,MO,MP,MQ,MR,MS,MT,MU,MV,MW,MX,MY,MZ ,NA,NB,NC,ND,NE,NF,NG,NH,NI,NJ,NK,NL,NM ,NN,NO,NP,NQ,NR,NS,NT,NU,NV,NW,NX,NY,NZ A Range Value CodArt B Descripcion C CodMarca D Descripcion Marca E Prod.Propios F Prod.Terceros G Importe H Cant.Distrib. I Impte.Distrib. J Cant.Produccion. K Impte.Produccion. L Cant.Compras. M Impte.Compras. almtabla Tablas de Almacen MK ' P DisplayAlerts SaveAs Quit Proceso Terminado UE-GEN-EXCEL FacTabla Tablas Generales Facturacion b-factabla lRowId REPEXPO BORRAR UE-GRABAR GENERAL Proceso Concluido.. UE-PROCESAR lStock lSumaStock lxAtender alm lSec lSuma lAlmAlimentadores lStkAlimentadores lAlmLosDemas lStkLosDemas Almmmate , >>,>>>,>>9.99 Almacen SI UE-VERIFICA-STOCK Llave01 Almacen principal a abastecer Almacenes a consultar Fecha Procesar Matg01 Idx01 tabl01 mate01 alm01 X  �'  �  �.      ' �8   ��      0         pcProp      ��      P         pcProp      ��      p         plCancel    �   ��      �         pcProcName  �   ��      �        
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
 hTarget X  ��      L        pcMessage       ��      p        pcMessage       ��      �        plEnabled             �     cType       �     6   �          �                  getObjectType   4	  L	  N	  ,          
   hReposBuffer    L        @  
   hPropTable  h        `  
   hBuffer           |  
   hTable  �  �     7             �                  adm-clone-props ?  @  A  B  D  E  F  G  H  I  J  K  N  Q  R  S  T              
   hProc             <        pcProcName  �  �  	   8     $      x                  start-super-proc    �  �  �  �  �  �  �  �  �  H  �     9                                   �  �  	     :                                   �  �  �  L	     ;                                   �  �  	  �	     <                                   �  T	  �	     =                                   �  �  �	  �	     >                                   �  �  �  �	  ,
     ?                                   �  �  �  �  �  �  �  �	  x
     @                                   �  �  �  �  H
  �
     A                                   �  �  �
       B               �
                  adm-create-objects  "  �
  D     C               8                  disable_UI  2  3  4  5    �     D               �                  enable_UI   E  G  J  K  T  �     E               �                  exitObject  X  Y  [            �     lFiler  �  H     F   �          4                  ue-cargar-articulos i  k  n  p  r  u  v  y  |  }    �  �  �    �     G               �                  ue-fecha-proceso    �  �  �  �  �  �  �        �     lFileXls                 lNuevoFile  8        0     lCosto  `        L     chExcelApplication  �        t     chWorkbook  �        �     chWorksheet �        �     chWorksheetRange    �     	   �     iCount        
   �     iIndex               iColumn 8        0     iRow    T        L     cColumn p        h     cRange  �        �     lCerrarAlTerminar   �        �     lMensajeAlTerminar           �  '   cColList    �     n   H   �                            ue-gen-excel    �  �  �  �  �  �  �  �  �  �  �  �  �  �  �                   	  
                                        !  "  $  '  (  )  +  ,  -  /  0  2  3  4  5  6  7  8  9  :  <  =  >  ?  A  B  C  E  F  I  J  K  L  O  P  Q  R  U  V  W  X  Z  ^  `  b  c  d  e  f  g  l  m  n  o  p  q  r  s  w  x  y  z  |  }  ~  �      "     �     lRowId       !  C    b-factabla  �  L     I   �      �  @                  ue-grabar   �  �  �  �  �  �  �  �  �  �  �  �  �  �    �  	   J               �                  ue-procesar �  �  �  �  �  �  �  �  �     $      �     lStock     $           lSumaStock  @  $      4     lxAtender   X  $      T     alm t  $      l     lSec    �  $      �     lSuma   �  $      �     lAlmAlimentadores   �  $   	   �     lStkAlimentadores     $   
   �     lAlmLosDemas        $           lStkLosDemas    �  l  ;   K   �          X                  ue-verifica-stock   �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �                                         !  "  #  $  %  )  +  -  .  0  2  3  4  5  6  8  :  (  4       D      X                          �  �     tt-factabla                                     $        ,        4        <        CodCia  Tabla   Codigo  Nombre  Valor   Campo-C Campo-D Campo-L d          X  
   appSrvUtils �        x     s-codcia    �       �     lFechaProceso   �       �  
   wWin    �       �     FILL-IN-1          �     txtAlmacen  (            txtAlmacenes    P        <  
   gshAstraAppserver   x        d  
   gshSessionManager   �        �  
   gshRIManager    �        �  
   gshSecurityManager  �  	 	     �  
   gshProfileManager     
 
        
   gshRepositoryManager    D        ,  
   gshTranslationManager   h        X  
   gshWebManager   �        |     gscSessionId    �        �     gsdSessionObj   �        �  
   gshFinManager   �        �  
   gshGenManager             
   gshAgnManager   @        0     gsdTempUniqueID `        T     gsdUserObj  �        t     gsdRenderTypeObj    �        �     gsdSessionScopeObj  �       �  
   ghProp  �       �  
   ghADMProps            
   ghADMPropsBuf   8    	   $     glADMLoadFromRepos  T    
   L     glADMOk t       h  
   ghContainer �       �     cObjectName �       �     iStart  �       �     cAppService �       �     cASDivision             cServerOperatingMode    8       0     cFields          L     iStartPage  t    L  h  tt-factabla �       �  VtaTabla    �       �  Almmmatg    �       �  RatioCab    �       �  almtabla        #   �  FacTabla       %      Almmmate         &    ,  Almacen          7   ;  <  M  |  }    �  �  �  �            (  )  *  ,  .  /  0  4  5  8  9  :  ;  =  ?  A  C  D  E  H  J  K  M  N  O  P  Q  W  Y  _  a  c  d  j  k  l  m  p  q  s  t  v  w  x  y  z  {  |  ~    �  �  �  �  �  �  �  n	  o	  r	  s	  t	  u	  v	  w	  x	  y	  z	  {	  |	  }	  ~	  	  �	   
  
  
  
  
  
  
  
  	
  

  
  
  
  
  
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
  M  Y  �  �  �  �  �  �  �  �  �  �  �  �    !  #  8  �  �  �  �          	  
      /  C  v  �  �  �    �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  ?  f  �  �  �  �  �  �  �  �  �  �      	  
              �i & d:\newsie\on_in_co\APLIC\lib\excel-close-file.i  D  X� % d:\newsie\on_in_co\APLIC\lib\excel-open-file.i   �  H� $ C:\Progress\OpenEdge\src\adm2\windowmn.i �  f!  C:\Progress\OpenEdge\src\adm2\containr.i �  � # %C:\Progress\OpenEdge\src\adm2\custom\containrcustom.i    $   ��  C:\Progress\OpenEdge\src\adm2\visual.i   h   # " %C:\Progress\OpenEdge\src\adm2\custom\visualcustom.i  �   �<  C:\Progress\OpenEdge\src\adm2\appserver.i    �   �� ! %C:\Progress\OpenEdge\src\adm2\custom\appservercustom.i   !  I�  C:\Progress\OpenEdge\src\adm2\smart.i    X!  Ds   C:\Progress\OpenEdge\gui\fn  �!  tw  %C:\Progress\OpenEdge\src\adm2\custom\smartcustom.i   �!  Q.  C:\Progress\OpenEdge\gui\set �!  ��  C:\Progress\OpenEdge\src\adm2\cntnprop.i "  ��  %C:\Progress\OpenEdge\src\adm2\custom\cntnpropcustom.i    P"  P  %C:\Progress\OpenEdge\src\adm2\custom\cntnprtocustom.i    �"  F>  C:\Progress\OpenEdge\src\adm2\visprop.i  �"  �I  %C:\Progress\OpenEdge\src\adm2\custom\vispropcustom.i #  ��  %C:\Progress\OpenEdge\src\adm2\custom\visprtocustom.i L#  �l 
 C:\Progress\OpenEdge\src\adm2\appsprop.i �#  ɏ  %C:\Progress\OpenEdge\src\adm2\custom\appspropcustom.i    �#  V  %C:\Progress\OpenEdge\src\adm2\custom\appsprtocustom.i    $  i$  C:\Progress\OpenEdge\src\adm2\smrtprop.i H$  �j  C:\Progress\OpenEdge\gui\get |$  �  %C:\Progress\OpenEdge\src\adm2\custom\smrtpropcustom.i    �$  ��  %C:\Progress\OpenEdge\src\adm2\custom\smrtprtocustom.i    �$  ��  C:\Progress\OpenEdge\src\adm2\smrtprto.i ,%  Su  C:\Progress\OpenEdge\src\adm2\globals.i  `%  M�  %C:\Progress\OpenEdge\src\adm2\custom\globalscustom.i �%  )a  %C:\Progress\OpenEdge\src\adm2\custom\smartdefscustom.i   �%  �  C:\Progress\OpenEdge\src\adm2\appsprto.i &  ��  %C:\Progress\OpenEdge\src\adm2\custom\appserverdefscustom.i   L&  �X 	 C:\Progress\OpenEdge\src\adm2\visprto.i  �&  !�  %C:\Progress\OpenEdge\src\adm2\custom\visualdefscustom.i  �&  n�  C:\Progress\OpenEdge\src\adm2\cntnprto.i '  ;  %C:\Progress\OpenEdge\src\adm2\custom\containrdefscustom.i    @'  ~�  C:\Progress\OpenEdge\src\adm2\widgetprto.i   �'  e�  !C:\Progress\OpenEdge\gui\adecomm\appserv.i   �'  �a   d:\newsie\on_in_co\APLIC\vta2\w-programacion-reposicion.w        �  �      @(     ]  &   P(  �  �      `(     �  %   p(  �         �(     �  $   �(  �   �      �(  �   �     �(     ^     �(  �   Y     �(     7     �(  �   /     �(     �  #    )  �   �     )     �       )  �   �     0)     �      @)  �   �     P)     �      `)  r   �     p)  n   ~     �)     &  "   �)  i   !     �)     �     �)  P   �     �)  �   �     �)     �  !   �)  �   �     �)     ^      *  �   ]     *     ;      *  �   9     0*          @*  g   �     P*     �     `*  O   �     p*  �   P     �*     N      �*  �        �*     �     �*  �   �     �*     �     �*  �   �     �*     v     �*  �   u      +     S     +  �   R      +     0     0+  �        @+     �     P+  �   �     `+     �     p+  }   �     �+     �     �+     .     �+     �     �+     �     �+  7   V     �+  �   M     �+  O   ?     �+     .      ,     �
     ,  �   �
      ,  �   �
     0,  O   �
     @,     p
     P,     "
     `,  �   �	     p,  x   �	  
   �,  M   �	     �,     �	     �,     �	     �,  a   l	  
   �,  �  K	     �,     ,	     �,  �  �     �,  O   �      -     �     -     �      -  �   �     0-     �     @-     �     P-  x   �     `-     �     p-     G     �-     C     �-     /     �-          �-  Q     
   �-     �     �-     t  
   �-     `     �-     F  
    .  f        .     �  	    .  "   v     0.     b     @.     A     P.  Z   �     `.     �     p.     �     �.     �     �.     �     �.     U     �.  '   �       �.     @      �.            �.           