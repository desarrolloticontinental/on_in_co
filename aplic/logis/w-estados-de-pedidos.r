	��VȤKf�5  ��              ;                                �� 35C0010Dutf-8 MAIN D:\newsie\on_in_co\aplic\logis\w-estados-de-pedidos.w,, PROCEDURE ue-sectores,,INPUT pCodDoc CHARACTER,INPUT pNroDoc CHARACTER,OUTPUT pSectores INTEGER,OUTPUT pSecImp INTEGER,OUTPUT pSecAsig INTEGER,OUTPUT pSecDev INTEGER PROCEDURE items-pesos,,OUTPUT pItems INTEGER,OUTPUT pPeso DECIMAL PROCEDURE initializeObject,, PROCEDURE exitObject,, PROCEDURE enable_UI,, PROCEDURE disable_UI,, PROCEDURE Carga-Temporal,, PROCEDURE adm-create-objects,, PROCEDURE start-super-proc,,INPUT pcProcName CHARACTER PROCEDURE adm-clone-props,, PROCEDURE toggleData,,INPUT plEnabled LOGICAL PROCEDURE showMessageProcedure,,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL PROCEDURE returnFocus,,INPUT hTarget HANDLE PROCEDURE repositionObject,,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL PROCEDURE removeLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE removeAllLinks,, PROCEDURE modifyUserLinks,,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE PROCEDURE modifyListProperty,,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER PROCEDURE hideObject,, PROCEDURE editInstanceProperties,, PROCEDURE displayLinks,, PROCEDURE createControls,, PROCEDURE changeCursor,,INPUT pcCursor CHARACTER PROCEDURE applyEntry,,INPUT pcField CHARACTER PROCEDURE adjustTabOrder,,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER PROCEDURE addMessage,,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER PROCEDURE addLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE unbindServer,,INPUT pcMode CHARACTER PROCEDURE startServerObject,, PROCEDURE runServerObject,,INPUT phAppService HANDLE PROCEDURE restartServerObject,, PROCEDURE initializeServerObject,, PROCEDURE disconnectObject,, PROCEDURE destroyServerObject,, PROCEDURE bindServer,, PROCEDURE processAction,,INPUT pcAction CHARACTER PROCEDURE enableObject,, PROCEDURE disableObject,, PROCEDURE applyLayout,, PROCEDURE viewPage,,INPUT piPageNum INTEGER PROCEDURE viewObject,, PROCEDURE toolbar,,INPUT pcValue CHARACTER PROCEDURE selectPage,,INPUT piPageNum INTEGER PROCEDURE removePageNTarget,,INPUT phTarget HANDLE,INPUT piPage INTEGER PROCEDURE passThrough,,INPUT pcLinkName CHARACTER,INPUT pcArgument CHARACTER PROCEDURE notifyPage,,INPUT pcProc CHARACTER PROCEDURE initPages,,INPUT pcPageList CHARACTER PROCEDURE initializeVisualContainer,, PROCEDURE hidePage,,INPUT piPageNum INTEGER PROCEDURE destroyObject,, PROCEDURE deletePage,,INPUT piPageNum INTEGER PROCEDURE createObjects,, PROCEDURE constructObject,,INPUT pcProcName CHARACTER,INPUT phParent HANDLE,INPUT pcPropList CHARACTER,OUTPUT phObject HANDLE PROCEDURE confirmExit,,INPUT-OUTPUT plCancel LOGICAL PROCEDURE changePage,, PROCEDURE assignPageProperty,,INPUT pcProp CHARACTER,INPUT pcValue CHARACTER FUNCTION Signature,CHARACTER,INPUT pcName CHARACTER FUNCTION showmessage,LOGICAL,INPUT pcMessage CHARACTER FUNCTION setUserProperty,LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION setUIBMode,LOGICAL,INPUT pcMode CHARACTER FUNCTION setTranslatableProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setSupportedLinks,LOGICAL,INPUT pcLinkList CHARACTER FUNCTION setRunAttribute,LOGICAL,INPUT cRunAttribute CHARACTER FUNCTION setPhysicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setPhysicalObjectName,LOGICAL,INPUT cTemp CHARACTER FUNCTION setPassThroughLinks,LOGICAL,INPUT pcLinks CHARACTER FUNCTION setParentDataKey,LOGICAL,INPUT cParentDataKey CHARACTER FUNCTION setObjectVersion,LOGICAL,INPUT cObjectVersion CHARACTER FUNCTION setObjectParent,LOGICAL,INPUT phParent HANDLE FUNCTION setObjectName,LOGICAL,INPUT pcName CHARACTER FUNCTION setLogicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setLogicalObjectName,LOGICAL,INPUT c CHARACTER FUNCTION setInstanceProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setDynamicObject,LOGICAL,INPUT lTemp LOGICAL FUNCTION setDesignDataObject,LOGICAL,INPUT pcDataObject CHARACTER FUNCTION setDBAware,LOGICAL,INPUT lAware LOGICAL FUNCTION setDataTargetEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setDataTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setDataSourceNames,LOGICAL,INPUT pcSourceNames CHARACTER FUNCTION setDataSourceEvents,LOGICAL,INPUT pcEventsList CHARACTER FUNCTION setDataSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDataLinksEnabled,LOGICAL,INPUT lDataLinksEnabled LOGICAL FUNCTION setContainerSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setContainerSource,LOGICAL,INPUT phObject HANDLE FUNCTION setContainerHidden,LOGICAL,INPUT plHidden LOGICAL FUNCTION setChildDataKey,LOGICAL,INPUT cChildDataKey CHARACTER FUNCTION reviewMessages,CHARACTER, FUNCTION propertyType,CHARACTER,INPUT pcPropName CHARACTER FUNCTION messageNumber,CHARACTER,INPUT piMessage INTEGER FUNCTION mappedEntry,CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER FUNCTION linkProperty,CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER FUNCTION linkHandles,CHARACTER,INPUT pcLink CHARACTER FUNCTION instancePropertyList,CHARACTER,INPUT pcPropList CHARACTER FUNCTION getUserProperty,CHARACTER,INPUT pcPropName CHARACTER FUNCTION getUIBMode,CHARACTER, FUNCTION getTranslatableProperties,CHARACTER, FUNCTION getSupportedLinks,CHARACTER, FUNCTION getRunAttribute,CHARACTER, FUNCTION getQueryObject,LOGICAL, FUNCTION getPropertyDialog,CHARACTER, FUNCTION getPhysicalVersion,CHARACTER, FUNCTION getPhysicalObjectName,CHARACTER, FUNCTION getPassThroughLinks,CHARACTER, FUNCTION getParentDataKey,CHARACTER, FUNCTION getObjectVersionNumber,CHARACTER, FUNCTION getObjectVersion,CHARACTER, FUNCTION getObjectParent,HANDLE, FUNCTION getObjectPage,INTEGER, FUNCTION getObjectName,CHARACTER, FUNCTION getObjectInitialized,LOGICAL, FUNCTION getObjectHidden,LOGICAL, FUNCTION getLogicalVersion,CHARACTER, FUNCTION getLogicalObjectName,CHARACTER, FUNCTION getInstanceProperties,CHARACTER, FUNCTION getDynamicObject,LOGICAL, FUNCTION getDesignDataObject,CHARACTER, FUNCTION getDBAware,LOGICAL, FUNCTION getDataTargetEvents,CHARACTER, FUNCTION getDataTarget,CHARACTER, FUNCTION getDataSourceNames,CHARACTER, FUNCTION getDataSourceEvents,CHARACTER, FUNCTION getDataSource,HANDLE, FUNCTION getDataLinksEnabled,LOGICAL, FUNCTION getContainerType,CHARACTER, FUNCTION getContainerSourceEvents,CHARACTER, FUNCTION getContainerSource,HANDLE, FUNCTION getContainerHidden,LOGICAL, FUNCTION getContainerHandle,HANDLE, FUNCTION getChildDataKey,CHARACTER, FUNCTION fetchMessages,CHARACTER, FUNCTION assignLinkProperty,LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION anyMessage,LOGICAL, FUNCTION setServerOperatingMode,LOGICAL,INPUT pcServerOperatingMode CHARACTER FUNCTION setServerFileName,LOGICAL,INPUT pcFileName CHARACTER FUNCTION setASUsePrompt,LOGICAL,INPUT plFlag LOGICAL FUNCTION setASInitializeOnRun,LOGICAL,INPUT plInitialize LOGICAL FUNCTION setASInfo,LOGICAL,INPUT pcInfo CHARACTER FUNCTION setASHandle,LOGICAL,INPUT phASHandle HANDLE FUNCTION setASDivision,LOGICAL,INPUT pcDivision CHARACTER FUNCTION setAppService,LOGICAL,INPUT pcAppService CHARACTER FUNCTION runServerProcedure,HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE FUNCTION getServerOperatingMode,CHARACTER, FUNCTION getServerFileName,CHARACTER, FUNCTION getASUsePrompt,LOGICAL, FUNCTION getASInitializeOnRun,LOGICAL, FUNCTION getASInfo,CHARACTER, FUNCTION getASHasStarted,LOGICAL, FUNCTION getASHandle,HANDLE, FUNCTION getAsDivision,CHARACTER, FUNCTION getASBound,LOGICAL, FUNCTION getAppService,CHARACTER, FUNCTION createUiEvents,LOGICAL, FUNCTION getObjectSecured,LOGICAL, FUNCTION getObjectTranslated,LOGICAL, FUNCTION setResizeVertical,LOGICAL,INPUT plResizeVertical LOGICAL FUNCTION setResizeHorizontal,LOGICAL,INPUT plResizeHorizontal LOGICAL FUNCTION setObjectLayout,LOGICAL,INPUT pcLayout CHARACTER FUNCTION setLayoutOptions,LOGICAL,INPUT pcOptions CHARACTER FUNCTION setHideOnInit,LOGICAL,INPUT plHide LOGICAL FUNCTION setDisableOnInit,LOGICAL,INPUT plDisable LOGICAL FUNCTION setDefaultLayout,LOGICAL,INPUT pcDefault CHARACTER FUNCTION setAllFieldNames,LOGICAL,INPUT pcValue CHARACTER FUNCTION setAllFieldHandles,LOGICAL,INPUT pcValue CHARACTER FUNCTION getResizeVertical,LOGICAL, FUNCTION getResizeHorizontal,LOGICAL, FUNCTION getWidth,DECIMAL, FUNCTION getRow,DECIMAL, FUNCTION getObjectLayout,CHARACTER, FUNCTION getObjectEnabled,LOGICAL, FUNCTION getLayoutVariable,CHARACTER, FUNCTION getLayoutOptions,CHARACTER, FUNCTION getHideOnInit,LOGICAL, FUNCTION getHeight,DECIMAL, FUNCTION getEnabledObjHdls,CHARACTER, FUNCTION getEnabledObjFlds,CHARACTER, FUNCTION getDisableOnInit,LOGICAL, FUNCTION getDefaultLayout,CHARACTER, FUNCTION getCol,DECIMAL, FUNCTION getAllFieldNames,CHARACTER, FUNCTION getAllFieldHandles,CHARACTER, FUNCTION setStatusArea,LOGICAL,INPUT plStatusArea LOGICAL FUNCTION getObjectType,character, FUNCTION setWindowTitleViewer,LOGICAL,INPUT phViewer HANDLE FUNCTION setWaitForObject,LOGICAL,INPUT phObject HANDLE FUNCTION setUpdateTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setUpdateSource,LOGICAL,INPUT pcSource CHARACTER FUNCTION setTopOnly,LOGICAL,INPUT plTopOnly LOGICAL FUNCTION setSdoForeignFields,LOGICAL,INPUT cSdoForeignFields CHARACTER FUNCTION setSavedContainerMode,LOGICAL,INPUT cSavedContainerMode CHARACTER FUNCTION setRunMultiple,LOGICAL,INPUT plMultiple LOGICAL FUNCTION setRunDOOptions,LOGICAL,INPUT pcOptions CHARACTER FUNCTION setRouterTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setReEnableDataLinks,LOGICAL,INPUT cReEnableDataLinks CHARACTER FUNCTION setPrimarySdoTarget,LOGICAL,INPUT hPrimarySdoTarget HANDLE FUNCTION setPageSource,LOGICAL,INPUT phObject HANDLE FUNCTION setPageNTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setOutMessageTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setNavigationTarget,LOGICAL,INPUT cTarget CHARACTER FUNCTION setNavigationSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setNavigationSource,LOGICAL,INPUT pcSource CHARACTER FUNCTION setMultiInstanceSupported,LOGICAL,INPUT lMultiInstanceSupported LOGICAL FUNCTION setMultiInstanceActivated,LOGICAL,INPUT lMultiInstanceActivated LOGICAL FUNCTION setInMessageTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setFilterSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDynamicSDOProcedure,LOGICAL,INPUT pcProc CHARACTER FUNCTION setDisabledAddModeTabs,LOGICAL,INPUT cDisabledAddModeTabs CHARACTER FUNCTION setCurrentPage,LOGICAL,INPUT iPage INTEGER FUNCTION setContainerTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setContainerMode,LOGICAL,INPUT cContainerMode CHARACTER FUNCTION setCallerWindow,LOGICAL,INPUT h HANDLE FUNCTION setCallerProcedure,LOGICAL,INPUT h HANDLE FUNCTION setCallerObject,LOGICAL,INPUT h HANDLE FUNCTION pageNTargets,CHARACTER,INPUT phTarget HANDLE,INPUT piPageNum INTEGER FUNCTION getStatusArea,LOGICAL, FUNCTION getWindowTitleViewer,HANDLE, FUNCTION getWaitForObject,HANDLE, FUNCTION getUpdateTarget,CHARACTER, FUNCTION getUpdateSource,CHARACTER, FUNCTION getTopOnly,LOGICAL, FUNCTION getSdoForeignFields,CHARACTER, FUNCTION getSavedContainerMode,CHARACTER, FUNCTION getRunMultiple,LOGICAL, FUNCTION getRunDOOptions,CHARACTER, FUNCTION getReEnableDataLinks,CHARACTER, FUNCTION getPrimarySdoTarget,HANDLE, FUNCTION getPageSource,HANDLE, FUNCTION getPageNTarget,CHARACTER, FUNCTION getOutMessageTarget,HANDLE, FUNCTION getNavigationTarget,HANDLE, FUNCTION getNavigationSourceEvents,CHARACTER, FUNCTION getNavigationSource,CHARACTER, FUNCTION getMultiInstanceSupported,LOGICAL, FUNCTION getMultiInstanceActivated,LOGICAL, FUNCTION getFilterSource,HANDLE, FUNCTION getDynamicSDOProcedure,CHARACTER, FUNCTION getDisabledAddModeTabs,CHARACTER, FUNCTION getCurrentPage,INTEGER, FUNCTION getContainerTargetEvents,CHARACTER, FUNCTION getContainerTarget,CHARACTER, FUNCTION getContainerMode,CHARACTER, FUNCTION getCallerWindow,HANDLE, FUNCTION getCallerProcedure,HANDLE, FUNCTION enablePagesInFolder,LOGICAL,INPUT pcPageInformation CHARACTER FUNCTION disablePagesInFolder,LOGICAL,INPUT pcPageInformation CHARACTER FUNCTION widgetValueList,CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION widgetValue,CHARACTER,INPUT pcName CHARACTER FUNCTION widgetIsTrue,LOGICAL,INPUT pcName CHARACTER FUNCTION widgetIsModified,LOGICAL,INPUT pcNameList CHARACTER FUNCTION widgetIsFocused,LOGICAL,INPUT pcName CHARACTER FUNCTION widgetIsBlank,LOGICAL,INPUT pcNameList CHARACTER FUNCTION widgetLongcharValue,LONGCHAR,INPUT pcName CHARACTER FUNCTION widgetHandle,HANDLE,INPUT pcName CHARACTER FUNCTION viewWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION toggleWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION resetWidgetValue,LOGICAL,INPUT pcNameList CHARACTER FUNCTION highlightWidget,LOGICAL,INPUT pcNameList CHARACTER,INPUT pcHighlightType CHARACTER FUNCTION hideWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION formattedWidgetValueList,CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION formattedWidgetValue,CHARACTER,INPUT pcName CHARACTER FUNCTION enableWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION enableRadioButton,LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER FUNCTION disableWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION disableRadioButton,LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER FUNCTION clearWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION blankWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION assignWidgetValueList,LOGICAL,INPUT pcNameList CHARACTER,INPUT pcValueList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION assignWidgetValue,LOGICAL,INPUT pcName CHARACTER,INPUT pcValue CHARACTER FUNCTION assignFocusedWidget,LOGICAL,INPUT pcName CHARACTER       4�                           G� 4�  ��               z              T=  
  +   � �  7   �� `  8    �   C   � �;  D   �= |  E   l?    F   �A $  G   �B �  H   �D 4  I   �L 8	  J           �U P  ? LZ �)  iSO8859-1                                                                           �   + �                                      �                  ��   
                 �U     �U   ں   ��  H�         |�  �   ��      �                                                       PROGRESS                         �           
    
                    �              �                                                                                                     
  T         �          L  ��  �   ��     �  ։�d�  �                     �W          �c      �                INTEGRAL                         PROGRESS                         @     �        �   C                      ։�d            �  ��                              �  �                      	  �  #�     CODCIACODDOCNROPEDFCHPEDCODCLINOMCLIDIRCLIRUCCLIHORAFLGESTCODALMCODMONTPOCMBUSUARIOGLOSAIMPBRTIMPEXOPORIGVIMPDTOIMPTOTIMPIGVCODVENIMPISCIMPVTAFLGSITFMAPGOFCHVENORDCMPCODDIVTIPVTAPORDTOTPOPEDUSRDSCTOLUGENTCODTRANSNROREFCMPBNTEOBSERVANCMPBNTEATENCIONFAXCLILUGENT2IMPFLEFLGIGVTPOLICUBIGEOACUBONNROCARDTIPBONIMPORTEPORCENTUSRAPROBACIONFCHAPROBACIONFCHENTCODPOSFLGENVUSRSACFECSACHORSACLIBRE_C01LIBRE_C02LIBRE_C03LIBRE_C04LIBRE_C05LIBRE_D01LIBRE_D02LIBRE_F01LIBRE_F02USRCHQFCHCHQHORCHQSEDEDIVDESCODREFIMPDTO2GLOSAIMPDTO2FLGIMPODUSRIMPODFCHIMPODUSRACTFECACTHORACTMOTREPOSICIONVTAPUNTUALCROSSDOCKINGALMACENXDCODORIGENNROORIGENDTALMACENDTEMPAQESPECCLIENTE_RECOGELISTA_DE_PRECIOSCUSTOMERPURCHASEORDERCUSTOMERREQUESTORDERTYPEPERIODCURRENCYPAYFORMDELIVERYDATEREGION1REGION1NAMEREGION2REGION2NAMEREGION3REGION3NAMETELEPHONECONTACTRECEPTORCONTACTRECEPTORNAMEDELIVERYADDRESSCUSTOMERLABELOFFICECUSTOMEROFFICECUSTOMERNAMECUSTOMERSTOCKDEPOCUSTOMERSTOCKDEPONAMECONSOLIDATEINVOICECUSTOMERINVOICECUSTOMERGROUPITEMSPESOVOLUMENEMBALAJE_ESPECIALDELIVERYGROUPDNICLIE-MAILIMPPERCEPCIONPORPERCEPCIONCODPAISCODDEPTCODPROVCODDISTREFERENCEADDRESSIDCUSTOMERFLAGMIGRACIONMIGFECHAMIGHORAMIGUSUARIOTOTALVALORVENTANETOOPGRAVADASTOTALVALORVENTANETOOPGRATUITASTOTALTRIBUTOSOPEGRATUITASTOTALIGVTOTALIMPUESTOSTOTALVALORVENTATOTALPRECIOVENTADESCUENTOSGLOBALESPORCENTAJEDSCTOGLOBALMONTOBASEDESCUENTOGLOBALTOTALVALORVENTANETOOPNOGRAVADATOTALDOCUMENTOANTICIPOMONTOBASEDSCTOGLOBALANTICIPOPORCENTAJEDSCTOGLOBALANTICIPOTOTALDSCTOGLOBALESANTICIPOMONTOBASEICBPERTOTALMONTOICBPERTOTALVALORVENTANETOOPEXONERADASTOTALVENTA                                                                       	          
                                                                                                                                                                                                                                       !          "          #          $          %          &          '          (          )          *          +          ,          -          .          /         0 
        1          2 
        3         4         5          6          7          8          9          :          ;          <          =          >          ?          @          A          B          C          D          E          F          G          H          I          J          K          L          M          N          O          P   "       Q          R          S          T          U          V          W          X          Y          Z          [          \          ]          ^          _          `          a          b          d          e          f          g          h          i          j          k          l          m          n          o          p          q          r          s          t          u          v          w          x          y          z          {          |          }          ~                    �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �             �                                                                                          �             8             $                                                                                                       �         �       �  L  @�  +   l�  �  ]      ��         �             P�          ��      �   L        �  
    
                  �  |             8                                                                                                    
  �        t  
    
                  `  (  	           �                                                                                                    
  �  +         
    
                    �  
           �                                                                                          +          
  P  8      �  
    
                  �  �             <                                                                                          8          
  �  K      x  
    
                  d  ,             �                                                                                          K          
  �  ]      $  
    
                    �             �                                                                                          ]          
  T  r      �  
    
                  �  �             @                                                                                          r          
     �      |  
    
                  h  0             �                                                                                          �          
  �  �      (                           �             �                                                                                          �            X  �      �                        �  �             D                                                                                          �              �      �  
    
                  l  4             �                                                                                          �          
  �  �      ,  
    
                    �             �                                                                                          �          
  \  �      �  
    
                  �  �             H                                                                                          �          
    �      �                        p  8             �                                                                                          �            �  �      0                          �             �                                                                                          �            `  �      �                        �  �             L                                                                                          �                      �                        t               �                                                                                                      �*     �        �                         ։�d            �&  ��                              �  �                      �#  �  #�     CODCIACODDOCNROPEDFCHPEDCODCLINOMCLIDIRCLIRUCCLIHORAFLGESTCODALMCODMONTPOCMBUSUARIOGLOSAIMPBRTIMPEXOPORIGVIMPDTOIMPTOTIMPIGVCODVENIMPISCIMPVTAFLGSITFMAPGOFCHVENORDCMPCODDIVTIPVTAPORDTOTPOPEDUSRDSCTOLUGENTCODTRANSNROREFCMPBNTEOBSERVANCMPBNTEATENCIONFAXCLILUGENT2IMPFLEFLGIGVTPOLICUBIGEOACUBONNROCARDTIPBONIMPORTEPORCENTUSRAPROBACIONFCHAPROBACIONFCHENTCODPOSFLGENVUSRSACFECSACHORSACLIBRE_C01LIBRE_C02LIBRE_C03LIBRE_C04LIBRE_C05LIBRE_D01LIBRE_D02LIBRE_F01LIBRE_F02USRCHQFCHCHQHORCHQSEDEDIVDESCODREFIMPDTO2GLOSAIMPDTO2FLGIMPODUSRIMPODFCHIMPODUSRACTFECACTHORACTMOTREPOSICIONVTAPUNTUALCROSSDOCKINGALMACENXDCODORIGENNROORIGENDTALMACENDTEMPAQESPECCLIENTE_RECOGELISTA_DE_PRECIOSCUSTOMERPURCHASEORDERCUSTOMERREQUESTORDERTYPEPERIODCURRENCYPAYFORMDELIVERYDATEREGION1REGION1NAMEREGION2REGION2NAMEREGION3REGION3NAMETELEPHONECONTACTRECEPTORCONTACTRECEPTORNAMEDELIVERYADDRESSCUSTOMERLABELOFFICECUSTOMEROFFICECUSTOMERNAMECUSTOMERSTOCKDEPOCUSTOMERSTOCKDEPONAMECONSOLIDATEINVOICECUSTOMERINVOICECUSTOMERGROUPITEMSPESOVOLUMENEMBALAJE_ESPECIALDELIVERYGROUPDNICLIE-MAILIMPPERCEPCIONPORPERCEPCIONCODPAISCODDEPTCODPROVCODDISTREFERENCEADDRESSIDCUSTOMERFLAGMIGRACIONMIGFECHAMIGHORAMIGUSUARIOTOTALVALORVENTANETOOPGRAVADASTOTALVALORVENTANETOOPGRATUITASTOTALTRIBUTOSOPEGRATUITASTOTALIGVTOTALIMPUESTOSTOTALVALORVENTATOTALPRECIOVENTADESCUENTOSGLOBALESPORCENTAJEDSCTOGLOBALMONTOBASEDESCUENTOGLOBALTOTALVALORVENTANETOOPNOGRAVADATOTALDOCUMENTOANTICIPOMONTOBASEDSCTOGLOBALANTICIPOPORCENTAJEDSCTOGLOBALANTICIPOTOTALDSCTOGLOBALESANTICIPOMONTOBASEICBPERTOTALMONTOICBPERTOTALVALORVENTANETOOPEXONERADASTOTALVENTA                                                                       	          
                                                                                                                                                                                                                                       !          "          #          $          %          &          '          (          )          *          +          ,          -          .          /         0 
        1          2 
        3         4         5          6          7          8          9          :          ;          <          =          >          ?          @          A          B          C          D          E          F          G          H          I          J          K          L          M          N          O          P   "       Q          R          S          T          U          V          W          X          Y          Z          [          \          ]          ^          _          `          a          b          d          e          f          g          h          i          j          k          l          m          n          o          p          q          r          s          t          u          v          w          x          y          z          {          |          }          ~                    �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �/      �&        �&                         �B�_            �&  ~                              �  x+                      l-  �+  �3     CODDIVDESDIVCODCIADIRDIVTELDIVFAXDIVRESPONFLGREPFLGPREUNIFLGAPRCOTFLGAPRPEDFLGPREVTADIASVTOCOTDIASVTOPEDDIASVTOO_DDIASAMPCOTTIPDTOMODPREUNIFLGEMPAQUEFLGROTACIONCANALVENTAFLGPICKINGFLGBARRASFLGMINVENTAFLGDTOPROMFLGDTOVOLFLGTARJETAVENTAMOSTRADORVENTAMAYORISTAVENTAMINORISTAFLGDTOCLFCLIPORDTOCLFCLIFLGDTOCNDVTALIBRE_C01LIBRE_C02LIBRE_D01LIBRE_D02LIBRE_F01LIBRE_F02LIBRE_L01LIBRE_L02FLAGTIPOFLAGUSUARIOFLAGMIGRACIONFLAGFECHACAMPO-LOGCAMPO-DECCAMPO-DATECAMPO-CHARGRUPO_DIVI_GGCENTRO_COSTO                                                                       	          
                                                                                                                                                                                                                                       !          "          #          $          %          &          '          (          )          *          +          ,          .          /          0 
        1 
        2 
        3 
        4          5          �0  !   @'        @'                         �B�_            I'  �e                              �  P0                      �0  `0  %      TABLACODIGONOMBRENOMANTCODCTA1CODCTA2                                                               3  "   f'        f'                         �B�_            f'  ��                              �  P1                      �1  `1  �      CODALMDESCRIPCIONCODCIATDOARTAUTMOVDIRALMHORRECENCALMTELALMCORRSALCORRINGCORRTRFCODDIVCLAVEALMCSGTPOCSGCODCLIFLGREPCAMPO-CALMPRINCIPALALMDESPACHOCAMPO-LOG                                                                        	          
                                                                                                     
                             
        �3  #   n'        n'                         �B�_            w'  ��                              �  �3                      �3  �3  &      CODDEPTOCODPROVICODDISTRNOMDISTRCODPOS                                                    (5  $   �'        �'                         �	b            �'  �                              �  x4                      �4  �4  4 	     CODCIACODDOCNRODOCCLAVEORDENCODIGOFECHAUSUARIOCODDIV                                                               "       	          
          �6  %   �'        �'                         �B�_            �'  �Q                              �  �5                      ,6  �5  r      CODCIACLAVEORDENCODIGONOMCORTONOMLARGOUSRCREACIONUSRMODIFICACIONUSRANULACIONFCHCREACIONFCHMODIFICACIONFCHANULACION                                                                        	          
             "          "          "       �>  &   �'        �'                         �B�_            �'  2W                              �  <7                       :  L7  �g     CODCIACODPEDNROPEDFCHPEDFCHVENCODCLINOMCLIDIRCLICODDIVLUGENTLUGENT2FCHENTSEDECODSEDRUCCLINROORDCODALMCODMONTPOCMBUSUARIOUSRAPROBACIONDNICLIUSRANUUSRMODUSRDSCTOFCHCREFCHANUFCHMODFCHDSCTOOBSERVAFLGIGVIMPBRTIMPEXOIMPIGVIMPDTOIMPTOTSDOACTIMPISCIMPVTAIMPFLEIMPINTIMPCTOPORDTOPORIGVPESMATMRGUTICODORINROORIFLGESTFLGSITFCHSITNROCARDFMAPGONROREFCODREFCODDEPTCODPROVCODDISTCODPOSFLGENVCMPBNTETPOLICCODVENHORALIBRE_C01LIBRE_C02LIBRE_C03LIBRE_C04LIBRE_C05LIBRE_D01LIBRE_D02LIBRE_F01LIBRE_F02GLOSACODTERIMPORTEFCHAPROBACIONFECSACHORSACUBIGEOUSRSACDIVDESUSRIMPODFCHIMPODUSRACTFECACTHORACTMOTREPOSICIONVTAPUNTUALORDCMPFLGIMPODZONAPICKEOUSRSACASIGNUSRSACRECEPUSUARIOINICIOUSUARIOFINITEMSPESOVOLUMENZONADISTRIBUCIONFCHFINFCHINICIOEMPAQESPEC                                                                      	          
                                                                                                                                                                             "          "          "          "       !          "          #          $          %          &          '          (          )          *          +          ,          -          .          /          0          1          2          3          4          5          6   "       7          8          9          :          ;          <          =          >          ?          @          A          B          C          D          E          F          G          H          I          J          K          L          M          N          O         P          Q          R          S         T          U          V          W   "       X          Y          Z          [          \          ]          ^          _          `          a          b          c          d          e          f          g          h   "       i   "       j          0H  (   y(        n(   C                      k�d            n(  M  I                           �  t?                      �C  �?  7_     CODDOCCODCIAFACTORUNDVTACODMATNROPEDCANPEDPREUNIPORDTOIMPDTOIMPLINNROITMAFTIGVAFTISCPREBASPREVTAIMPIGVIMPISCCANATEHORAFLGESTFCHPEDCODAUXMRGUTIPORDTO2TIPVTAPESMATCODCLIALMDESPOR_DSCTOSFLG_FACTORCODDIVCANPICKLIBRE_C01LIBRE_C02LIBRE_C03LIBRE_C04LIBRE_C05LIBRE_D01LIBRE_D02LIBRE_F01LIBRE_F02CANSOLCANAPRIMPDTO2ALMTRFCANTRFLIBRE_D03LIBRE_D04LIBRE_D05LIBRE_F05LIBRE_F03LIBRE_F04SDOCOTCODMATWEBDESMATWEBCANPEDWEBPREUNIWEBIMPLINWEBCUSTOMERARTCODECUSTOMERARTDESCRIPTIONCUSTOMERUNITCODECUSTOMERUNITCODENAMEQTYPRICEROWTOTALCUSTOMERCURRENCYCUSTOMEROLDARTCODEFACTORDESCUENTOTASAIGVIMPORTEUNITARIOSINIMPUESTOIMPORTEREFERENCIALIMPORTEBASEDESCUENTOIMPORTEDESCUENTOIMPORTETOTALSINIMPUESTOMONTOBASEIGVIMPORTEIGVIMPORTETOTALIMPUESTOSIMPORTEUNITARIOCONIMPUESTOCIMPORTEVENTAEXONERADOCIMPORTEVENTAGRATUITOCOTROSTRIBUTOSOPGRATUITOCTIPOAFECTACIONCPREUNISINIMPUESTOCSUMAIMPTETOTALSINIMPUESTOCMONTOBASEIGVCSUMAIGVIMPUESTOBOLSAPLASTICOMONTOTRIBUTOBOLSAPLASTICOCANTIDADBOLSAPLASTICOMONTOUNITARIOBOLSAPLASTICOCIMPORTETOTALCONIMPUESTOIMPORTEBASEDESCUENTONOAFECTOFACTORDESCUENTONOAFECTOIMPORTEDESCUENTONOAFECTO                                                                       	          
                                                                                                                                                                                                                                     !          "          #          $          %          &          '          (          )          *          +          ,          -          .          /          0          1          2          3          4          5          6          7          8          9          :          ;          <          =          >          ?          @          A          B          C          D          E          S          T          U          V          W          X          Y          Z          [          \          ]          ^          _          `          a          b          c          d          e          f          g          h          i          j          k          l          m              )   �(        �(                         �c            �(  �                              �  �H                      N  �H  P�     CODMATDESMATCODMARUNDSTKUNDCMPFACEQUCODCTACODNEWMONVTAPREVTAPREBASAFTIGVVINMN1CODCIAVINMN2CODFAMVCTMN1FCHACTCODPR1CODPR2VCTMN2ARTPROFCHUSALFCHUCMPPMAXMN1PMAXMN2PULTMN1PULTMN2USUARIOFCHINGFCHCESFCHALZCLFMATUNDBASSUBFAMCODBRRCODANTTIPARTFCHPRMDFCHPRMHFCHREAPESMATDETALLECANEMPALMACENESDESMARAFTISCPORISCPORVTATPOMRGCTOLISCTOPRMMRGUTIPORMAXFCHMPREUNDANTPREANTPREACTDSCTOSTPOPROPORIGVCTOTOTTPOSUMCTOUNDORDENORDLISORDTMPTPOARTTPOCMBPPCHR__01CHR__02CHR__03DEC__01DEC__02DEC__03DATE__01DATE__02DATE__03MRGUTI-AMRGUTI-BMRGUTI-CPREOFIUNDAUNDBUNDCFLGINTFLGPRECLASEFCHPROMCATCONTATIPROTDSCTOPROMINFORFLGINFORPROMDIVIPROMFCHDPROMFCHHPROMDTODTOVOLRDTOVOLDUNDALTDSCALTMRGALTPREALTLICENCIAPROMMINDIVIPROMMINFCHDPROMMINFCHHPROMMINDTOCODDIGESAVTODIGESALIBRE_C01LIBRE_C02LIBRE_C03LIBRE_C04LIBRE_C05LIBRE_D01LIBRE_D02LIBRE_F01LIBRE_F02STKMINSTKMAXSTKREPDESCRIPCION-LARGADESCRIPCION-TECNICASW-WEBWEB-SUBCATEGORIALIBRE_D03LIBRE_D04LIBRE_D05PESOBRUTOPAQUETELARGOALTOANCHOCTOLISMARCOCTOTOTMARCOCODSSFAMCLFESPECIALFLGCOMERCIALLIBRE_C06LIBRE_C07LIBRE_C08LIBRE_C09LIBRE_C10CODIGOPADREFACTORPADREREQUIERESERIALNRREQUIEREDUEDATEDTOVOLPTASAIMPUESTOIMPORTEUNITARIOSINIMPUESTODTOVOLPSINIMPUESTOIMPORTEUNITARIOSINIMPUESTO_AIMPORTEUNITARIOSINIMPUESTO_BIMPORTEUNITARIOSINIMPUESTO_CDTOVOLPIMPUESTOIMPORTEUNITARIOIMPUESTOIMPORTEUNITARIOIMPUESTO_AIMPORTEUNITARIOIMPUESTO_BIMPORTEUNITARIOIMPUESTO_C                                                                      	          
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
        �          �          �          �                        H�                                               L�           W  hW  H XV            
                                                     
             
             
                                         
                                                                                                                H   X   h   x   �   �   �   �   �   �   �   �       (  8  H      H   X   h   x   �   �   �   �   �   �   �   �      (  8  H                                                                                                                                     	                  
                                                                                                                                                                                                                                                                                                                                                                                                                               !                  "                  #                  $                  %                  &                  '                  (                  )                  *                  +                  ,                  -                  .                 /              
   0                  1              
   2                 3                 4                  5                  6                  7                  8                  9                  :                  ;                  <                  =                  >                  ?                  @                  A                  B                  C                  D                  E                  F                  G                  H                  I                  J                  K                  L                  M                  N                  O             "     P                  Q                  R                  S                  T                  U                  V                  W                  X                  Y                  Z                  [                  \                  ]                  ^                  _                  `                  a                  b                  c                  d                  e                  f                  g                  h                  i                  j                  k                  l                  m                  n                  o                  p                  q                  r                  s                  t                  u                  v                  w                  x                  y                  z                  {                  |                  }                  ~                                    �                  �                  �                  �                  �                  �                  �                  �                  �                  �                  �                  �                  �                  �                  �                  �                  �                  �                  �                  �                  �                  �                  �                  �                  �                  �                  �                  �                                 p~  x~  |~  �~  �~          �~             �~  �~  �~  �~  �~                         �~  �~  �~  �~  �~                         �~  �~                                  (  4  X  H                          \  h  p  �                              �  �  �  �  �          �             �  �  �  �  �          �             �  �  �  �  �           �              8�  @�  H�  P�  L�          T�              ��  ��  ��  ��  ��                          ��  ��  Ā  Ԁ  ̀                         �  �  ��  �  �                         �   �  (�  8�  0�          <�             P�  X�  \�  l�  d�          p�              ��  ��  ��  ��  ��          ��             ȁ  Ё  ؁  �  ��                         �  �  ��  �  �                          �  �   �  @�  0�                          D�  L�  \�  |�  l�                         ��  ��  ��  ��  ��                         Ă  ̂  Ԃ  �  ��                         ��  ��  �  0�  �                         4�  <�  L�  l�  \�                         p�  x�  ��  ��  ��                         ��  ��  ��  ԃ  ȃ                          ؃  ��  ��  �  ��                         �  �  $�  <�  0�                         @�  H�  X�  x�  h�                         |�  ��  ��  ��  ��                         ��  ��  ��  ܄  Ȅ                          ��  �  ��  �  ��          �             0�  8�  @�  X�  L�                          \�  d�  l�  ��  x�                         ��  ��  ��  ��  ��                         ��  ��  ��  �  Ѕ                          �  ��  ��   �  �                          $�  0�  8�  X�  H�          \�              l�  t�  |�  ��  ��                         ��  ��  ��  ܆  Ȇ                          ��  �  �  �  ��                          �  �  �  4�  (�                          8�  @�  H�  P�  L�          T�              d�  l�  t�  ��  |�                          ��  ��  ��  ��  ��                          ��  ��  ć  ܇  Ї          ��              �  ��  �  �  �                          �  (�  0�  @�  8�                          D�  L�  P�  `�  X�                          d�  l�  |�  ��  ��                         ��  ��  ��  ��  ��                         ��  Ĉ  ̈  ܈                              ��  ��  ��  �                              �  �  $�  4�                              <�  D�  L�  T�                              X�  `�  h�  ��                              ��  ��  ��  ��                              ��  ��  ��  ��                              ĉ  ̉  ԉ  ܉                              ��  �  �   �                              �  �  �  $�                              (�  4�  <�  H�                              L�  X�  `�  l�                              p�  |�  ��  ��                              ��  ��  ��  ��                             Ċ  Њ  �  ��                             �   �  �  �                              �  (�  4�  @�                              D�  L�  T�  `�                              d�  l�  x�  ��                             ��  ��  ��  ��                              ��  ��  ��  ��                              ��  ċ  ̋  ԋ                             ؋  ��  �  �                             ��   �  �  �                             �  ,�  4�  D�                              H�  T�  \�  `�                              d�  p�  x�  ��                              ��  ��  ��  ��                              ��  Č  ̌  Ԍ                              ،  ��  �  �                              ��   �  �  �                              �  $�  ,�  <�                              @�  L�  T�  `�                              d�  t�  |�  ��                              ��  ��  ��  ��                              ��  ��  ȍ  ԍ                             ؍  �  �  ��                             ��   �  �  �                              �  �  $�  0�                              4�  @�  H�  T�                              X�  h�  p�  ��                              ��  ��  ��  ��                              ��  Ў  ؎  �                             �  ��  �  �                             �  (�  0�  <�                              @�  H�  P�  X�                              \�  h�  p�  x�                              |�  ��  ��  ��                              ��  ��  ��  Џ                              ԏ  ܏  �  �                              ��  �  �  �                              �  $�  ,�  8�                              <�  H�  P�  \�                              `�  h�  p�  |�                              ��  ��  ��  ��                              ��  ��  Ȑ  ܐ                              ��  ��  ��  �                              �  $�  ,�  <�                              @�  P�  X�  h�                              l�  |�  ��  ��                              ��  ��  ��  ȑ                              ̑  ��  �  ��                              ��  �  �  ,�                              0�  L�  X�  t�                              x�  ��  ��  ��                             ��  ��  ̒  Ԓ                              ؒ  ��  �  ��                             ��  �  �  �                              �  4�  <�  P�                              T�  d�  l�  ��                              ��  ��  ��  ��                              ��  ��  ��  ��                              ��  ȓ  ؓ  �                             �  ��  �  �                              �  (�  0�  8�                              <�  D�  L�  \�                              `�  h�  p�  |�                              ��  ��  ��  ��                              ��  ��  ��  Д                              Ԕ  ��  �  ��                              ��  �  �  �                               �  ,�  8�  D�                              H�  P�  X�  `�                              d�  p�  x�  ��                              ��  ��  ��  ؕ                             ܕ  ��  �  ,�                             0�  L�  \�  x�                             |�  ��  ��  ��                             ��  ��  Ȗ  ؖ                             ܖ  �  ��  �                             �  $�  4�  H�                             L�  `�  p�  ��                             ��  ��  ��  ė                             ȗ  �  ��  �                             �  4�  D�  d�                             h�  ��  ��  ��                             ��  ̘  ܘ  ��                              �   �  ,�  L�                             P�  l�  |�  ��                             ��  ��  ��  ̙                             Й  �  ��  �                             �  ,�  <�  \�                             `�  l�  |�  ��                                                                         CodCia  999 Cia Cia 0   C�digo de Compa�ia  CodDoc  x(3)    Codigo  Codigo      NroPed  X(12)   No. Pedido  Numero!Pedido       FchPed  99/99/9999  Fecha   Fch.Pedido  today   fchven  99/99/99    Fecha Vencimiento   Fecha!Vcmto.    ?   UsrDscto    X(10)   Resp.!Dscto.        CodCli  x(11)   Codigo  Codigo      C�digo del Cliente  NomCli  x(100)  Nombre  Nombre      Nombre del Cliente  DirCli  x(100)  Direcci�n   Direcci�n       Direcci�n del Cliente   RucCli  x(20)   Ruc Ruc     Registro Unico de Contribuyente (Cliente)   ordcmp  X(12)   Orden de Compra Orden ! Compra      Hora    X(5)    Hora    Hora    string(TIME,"HH:MM")    TpoPed  X(1)    Tipo Pedido Tipo Pedido     CodAlm  x(3)    Almac�n Almac�n     C�digo de almac�n   CodMon  9   Moneda  Cod!mon 1   C�digo de moneda    TpoCmb  Z,ZZ9.9999  Tipo de cambio  T/Cambio    0   Tipo de cambio  usuario x(10)   usuario usuario     Glosa   X(50)   Glosa   Glosa       Observa X(50)   Observaciones   Observaciones       ImpBrt  ->>,>>>,>>9.99  Importe Bruto   Importe Bruto   0   ImpExo  ->>,>>>,>>9.99  Importe Exonerado   Importe Exonerado   0   PorIgv  ->>9.99 % I.G.V.    % I.G.V.    0   ImpDto  ->>,>>>,>>9.99  Importe Descuento   Importe Descuento   0   ImpTot  ->>,>>>,>>9.99  Importe Total   Importe Total   0   ImpIgv  ->>,>>>,>>9.99  Importe I.G.V.  Importe I.G.V.  0   CodVen  x(10)   Vendedor    Vendedor        ImpIsc  ->>,>>>,>>9.99  Importe Isc Importe Isc 0   ImpVta  ->>,>>>,>>9.99  Valor Venta Valor venta 0   ImpFle  ->>,>>>,>>9.99  Importe Flete   Importe Flete   0   FlgSit  X   Situaci�n   Situaci�n       FmaPgo  X(8)    Condicion de ventas Condicion de!venta      CodDiv  x(5)    Division    Division    00000   Ingrese el Codigo de Division   TipVta  X(1)    Tipo Venta  Tipo venta      PorDto  >>9.99  % Dscto.    % Dscto.    0   FlgEst  X(1)    Estado  Estado  P   LugEnt  x(60)   Lugar de entrega    Lugar de entrega        LugEnt2 x(60)   Lugar de entrega    Lugar de entrega        CodTrans    X(8)    Transportista   TRANSPORTISTA       TRANSPORTISTA   NroRef  X(12)   No. Referencia  Numero!Referencia       Cmpbnte X(3)    Tipo Comprobante    Tipo Comprobante    FAC NCmpbnte    X(9)    Numero  Numero      Atencion    X(30)   Atenci�n    Atenci�n        FaxCli  x(13)   Fax Fax     Fax del Cliente FlgIgv  Si/no   Con IGV Con IGV Si  TpoLic  si/no   Licitacion  Licitacion  no  Ubigeo  x(15)   Ubicaci�n   Ubicaci�n       C�digo de ubicaci�n AcuBon  ->>>,>>>,>>9.99 AcuBon  AcuBon  0   NroCard x(8)    NroCard Nrocard     TipBon  99  TipBon  TipBon  0   Importe >,>>>,>>9.99    Importe Importe 0   Porcent ->>9.99 Porcent Porcent 0   UsrAprobacion   x(10)   UsrAprobacion       FchAprobacion   99/99/99    FchAprobacion   ?   FchEnt  99/99/9999  Fecha Entrega   TODAY   CodPos  x(3)    Postal      FlgEnv  Si/No   El pedido es para enviar?   No  UsrSac  x(8)    Sacador     FecSac  99/99/99    Fecha   ?   HorSac  x(8)    Hora        Libre_c01   x(60)   Libre_c01       Libre_c02   x(60)   Libre_c02       Libre_c03   x(60)   Libre_c03       Libre_c04   x(60)   Libre_c04       Libre_c05   x(60)   Libre_c05       Libre_d01   ->>>,>>>,>>9.99<<<  Libre_d01   0   Libre_d02   ->>>,>>>,>>9.99<<<  Libre_d02   0   Libre_f01   99/99/99    Libre_f01   ?   Libre_f02   99/99/99    Libre_f02   ?   UsrChq  x(8)    Chequeador      FchChq  99/99/99    FchChq  ?   HorChq  x(8)    HorChq      Sede    x(5)    Sede        DivDes  x(5)    Destino     CodRef  x(3)    Referencia      ImpDto2 ->>>,>>>,>>9.99 ImpDto2 0   GlosaImpDto2    x(30)   GlosaImpDto2        FlgImpOD    Si/No       No  UsrImpOD    x(8)    UsrImpOD        FchImpOD    99/99/9999 HH:MM:SS.SSS FchImpOD    ?   UsrAct  x(8)    UsrAct      FecAct  99/99/99    FecAct  ?   HorAct  x(5)    HorAct      MotReposicion   x(8)    MotReposicion       VtaPuntual  yes/no  VtaPuntual  no  CrossDocking    yes/no  CrossDocking    no  AlmacenXD   x(10)   AlmacenXD       CodOrigen   x(8)    CodOrigen       NroOrigen   x(15)   NroOrigen       DT  yes/no  DT  no  AlmacenDT   x(10)   AlmacenDT       EmpaqEspec  yes/no  EmpaqEspec  no  Cliente_Recoge  yes/no  Cliente_Recoge  no  Lista_de_Precios    x(8)    Lista_de_Precios        CustomerPurchaseOrder   x(20)   Pedido Compra       CustomerRequest x(20)   Solicitud Pedido        OrderType   x(3)    Tipo Pedido     Period  x(10)   Periodo     Currency    x(3)    Moneda      PayForm x(10)   Forma de Pago       DeliveryDate    99/99/9999  Fecha de Entrega    ?   Region1 x(6)    Departamento        Region1Name x(10)   Region1Name     Region2 x(6)    Provincia       Region2Name x(10)   Region2Name     Region3 x(6)    Distrito        Region3Name x(10)   Region3Name     TelephoneContactReceptor    x(40)   Telefono Contacto       ContactReceptorName x(50)   Nombre del Contacto     DeliveryAddress x(200)  DeliveryAddress     CustomerLabel   x(30)   Centro de Costo     OfficeCustomer  x(10)   Codigo Centro       OfficeCustomerName  x(50)   Nombre de Centro        CustomerStockDepo   x(10)   Codigo Almacen      CustomerStockDepoName   x(50)   Nombre Almacen      ConsolidateInvoiceCustomer  ->,>>>,>>9  Consolidar Factura Cliente  0   InvoiceCustomerGroup    x(45)   Grupo de Factura de Client      Items   ->,>>>,>>9  Items   0   Peso    ->>,>>9.9999    Peso    0   Volumen ->>,>>9.9999    Volumen 0   Embalaje_Especial   yes/no  Embalaje_Especial   no  DeliveryGroup   x(10)   Grupo de Reparto        DNICli  x(20)   DNI     e-mail  x(200)  e-mail      ImpPercepcion   ->>>,>>>,>>9.99 ImpPercepcion   0   PorPercepcion   ->>>,>>>,>>9.99 PorPercepcion   0   CodPais x(3)    Pais        CodDept x(3)    Departamento        CodProv x(3)    Provincia       CodDist x(3)    Distrito        ReferenceAddress    x(200)  ReferenceAddress        IDCustomer  x(11)   IDCustomer      FlagMigracion   x   FlagMigracion       MigFecha    99/99/9999  MigFecha    ?   MigHora x(10)   MigHora     MigUsuario  x(10)   MigUsuario      TotalValorVentaNetoOpGravadas   >>>>>>>>>>>9.99 TotalValorVentaNetoOpGravadas   0   TotalValorVentaNetoOpGratuitas  >>>>>>>>>>>9.99 TotalValorVentaNetoOpGratuitas  0   TotalTributosOpeGratuitas   >>>>>>>>>>>9.99 TotalTributosOpeGratuitas   0   TotalIGV    >>>>>>>>>>>9.99 TotalIGV    0   TotalImpuestos  >>>>>>>>>>>9.99 TotalImpuestos  0   TotalValorVenta >>>>>>>>>>>9.99 TotalValorVenta 0   TotalPrecioVenta    >>>>>>>>>>>9.99 TotalPrecioVenta    0   DescuentosGlobales  >>>>>>>>>>>9.99 DescuentosGlobales  0   PorcentajeDsctoGlobal   >>9.99999   PorcentajeDsctoGlobal   0   MontoBaseDescuentoGlobal    >>>>>>>>>>>9.99 MontoBaseDescuentoGlobal    0   TotalValorVentaNetoOpNoGravada  >>>>>>>>>>>9.99 TotalValorVentaNetoOpNoGravada  0   TotalDocumentoAnticipo  >>>>>>>>>>>9.99 TotalDocumentoAnticipo  0   MontoBaseDsctoGlobalAnticipo    >>>>>>>>>>>9.99 MontoBaseDsctoGlobalAnticipo    0   PorcentajeDsctoGlobalAnticipo   >>9.99999   PorcentajeDsctoGlobalAnticipo   0   TotalDsctoGlobalesAnticipo  >>>>>>>>>>>9.99 TotalDsctoGlobalesAnticipo  0   MontoBaseICBPER >>>>>>>>>>>9.99 MontoBaseICBPER 0   TotalMontoICBPER    >>>>>>>>>>>9.99 TotalMontoICBPER    0   TotalValorVentaNetoOpExoneradas >>>>>>>>>>>9.99 TotalValorVentaNetoOpExoneradas 0   TotalVenta  >>>>>>>>>>>9.99 TotalVenta  0   �  % 6 W � � � � � ��  ��� �������   ��      strin                  00000  P    FAC    �     ��           � �           ��    ��    � ��   �        �� �        � �                  �                                �                          1)        9)        A)        I)        Q)        Y)        a)        i)        q)        y)        �)        �)        �)        �)        �)                �     i  i  i      i  i  i  i      i  i  i  i      i  i  i  i      i  i  i  i  i      i  i  i  i      i 	 i 
 i  i      i  i  i  i  i      i  i  i  i  i  i  i  i      i  i  i  i  i      i  i  i      i  i  i  i      i  i  i  i      i  i  i  i      i  i  i     	 	 	 	! 	$ 		 	 	 	K 	( 	 	J 	 	 	_ 	` 	u 	G 	 	X 	Y 	    +   2   9   @   G   N   W   ^   e   l   s   z      �   �   �   �   �   �   �   �   �   �   �   �   �   �   �   �   �   �           !  (  0  9  @  H  Q  Z  a  h  o  v  }  �  �  �  �  �  �  �  �  �  �  �  �  �  �     
      (  2  <  C  J  Q  V  ]  d  l  y  �  �  �  �  �  �  �  �  �  �  �  �  �  �      %  ;  K  U  \  e  m  z  �  �  �  �  �  �  �  �  �      #  5  K  f  {  �  �  �  �  �  �  �  �  �  �  �  �  �  	    "  +  3  >  \  {  �  �  �  �  �  �  �    /  F  c  �  �  �  �  �                                                                                                                                     	                  
                                                                                                                         "                                                                                                              ��  ��  ��  ��                              ��  ��  ȥ  إ                              ܥ  �  �  ��                              ��   �  �  �                              �   �  (�  0�                              4�  <�  D�  L�                              P�  \�  d�  p�                              t�  ��  ��  ��                              ��  ��  ��  ��                              ��  ��  Ȧ  Ц                              Ԧ  ܦ  �  ��                               �  �  �  �                              �  $�  ,�  4�                              8�  @�  L�  X�                              \�  h�  p�  |�                              ��  ��  ��  ��                              ��  ȧ  ̧  ԧ                              ا  �  �  �                              ��  ��  �  �                               �  (�  0�  <�                                                                          Situacion   x(30)   Situaci�n       SitPedido   x(30)   Estado Pedido       NroHPK  x(15)   Nro HPK     SitHPK  x(30)   Estado HPK      CodPed  x(3)    C�digo      NroPed  x(15)   N�mero      CodOrigen   x(15)   Cod. Origen     NroOrigen   x(20)   Det. Origen     FchPed  99/99/9999  Emisi�n ?   Hora    x(8)    Hora        FchEnt  99/99/9999  Fecha de Entrega    ?   Origen  x(5)    Origen      NomCli  x(50)   Cliente     Peso    >>>,>>9.99  Peso Kg.    0   UsrImpOD    x(8)    Impreso por     FchImpOD    99/99/9999 HH:MM    Fecha y hora de impresi�n   ?   Items   >>9 Items   0   Glosa   x(50)   Glosa       ImpRef  ->>>,>>>,>>9.99 Importe Ref.    0   nomPos  x(40)   Distrito        �  �  ���������        � �    �    �     �)                �     i     	    �  �  �  �  �  9   �  �  @   z   �  �  ^   �  �  �  {  �   �  �    ��                                               '	          ����                            i)   ��    �)    ��    A)         A)   ��    1)   ��    �)  ! �y    �)  " !�    �)  # ��    �)  $ �    �)  % }k    �)  & q�    �)  $ �]    1)  ( �    �)  ) ��    �)  & ��    undefined                                                               �       d�  �   l   t�    ��                  �����               �j�                    O   ����    e�          O   ����    R�          O   ����    ��      t       �   �              4   ����     /                                    3   ����       $     H  ���                       8      
                       � ߱        �  �      D       p
     B          assignFocusedWidget         �      �           LOGICAL,INPUT pcName CHARACTER  assignWidgetValue   �      �      $    "      LOGICAL,INPUT pcName CHARACTER,INPUT pcValue CHARACTER  assignWidgetValueList         \      �    4      LOGICAL,INPUT pcNameList CHARACTER,INPUT pcValueList CHARACTER,INPUT pcDelimiter CHARACTER  blankWidget t      �          J      LOGICAL,INPUT pcNameList CHARACTER  clearWidget �      @      l    V      LOGICAL,INPUT pcNameList CHARACTER  disableRadioButton  L      �      �    b      LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER    disableWidget   �            4    u      LOGICAL,INPUT pcNameList CHARACTER  enableRadioButton         X      �    �      LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER    enableWidget    l      �      �    �      LOGICAL,INPUT pcNameList CHARACTER  formattedWidgetValue    �             X  	  �      CHARACTER,INPUT pcName CHARACTER    formattedWidgetValueList    8      |      �  
  �      CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER    hideWidget  �      �      (   
 �      LOGICAL,INPUT pcNameList CHARACTER  highlightWidget       L      |    �      LOGICAL,INPUT pcNameList CHARACTER,INPUT pcHighlightType CHARACTER  resetWidgetValue    \      �      �    �      LOGICAL,INPUT pcNameList CHARACTER  toggleWidget    �            H    �      LOGICAL,INPUT pcNameList CHARACTER  viewWidget  (      l      �   
 	      LOGICAL,INPUT pcNameList CHARACTER  widgetHandle    x      �      �          HANDLE,INPUT pcName CHARACTER   widgetLongcharValue �            @    !      LONGCHAR,INPUT pcName CHARACTER widgetIsBlank          `      �    5      LOGICAL,INPUT pcNameList CHARACTER  widgetIsFocused p      �      �    C      LOGICAL,INPUT pcName CHARACTER  widgetIsModified    �      	      8	    S      LOGICAL,INPUT pcNameList CHARACTER  widgetIsTrue    	      \	      �	    d      LOGICAL,INPUT pcName CHARACTER  widgetValue l	      �	      �	    q      CHARACTER,INPUT pcName CHARACTER    widgetValueList �	      �	      ,
    }      CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER    �    b  �
  �
  P  x       4   ����x       o   c       �
                              �  �   NA  �   �  �   �  �      �      �         $    8    L    `  `  t  
`  �  $  �    �     �      $  t  |  ���                       �     
                    � ߱        ؁    �  �  @      �      4   �����                P                      ��                  �  �                  h��                       �  �  �    �  l  |            4   ����      $  �  �  ���                       d  @         P              � ߱              �  �         �      4   �����      $  �  ,  ���                       �  @         �              � ߱        assignPageProperty                              �  �      ��                  +  .                H`�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   T                             ��                  H           ��                            ����                            changePage                              @  (      ��                  0  1  X              h��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            confirmExit                             @  (      ��                  3  5  X              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  p           ��                            ����                            constructObject                             l  T      ��                  7  <  �              ���                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             �               �� 
  �             �  
             ��                 �               �� 
                   
         ��                            ����                            createObjects                                 �      ��                  >  ?  (              dr�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            deletePage                                �      ��                  A  C  (              s�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  @           ��                            ����                            destroyObject                               <  $      ��                  E  F  T              X��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hidePage                                <  $      ��                  H  J  T              ���                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  l           ��                            ����                            initializeObject                                l  T      ��                  L  M  �              <��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeVisualContainer                               |  d      ��                  O  P  �              �]�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initPages                               |  d      ��                  R  T  �              ^�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            notifyPage                              �  �      ��                  V  X  �              �^�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            passThrough                             �  �      ��                  Z  ]  �              H(�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   0             �               ��                  $           ��                            ����                            removePageNTarget                               $        ��                  _  b  <              �                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �             T  
             ��                  |           ��                            ����                            selectPage                              t  \      ��                  d  f  �              T$�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            toolbar                             �  �      ��                  h  j  �              L)�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            viewObject                              �   �       ��                  l  m  �               �                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            viewPage                                �!  �!      ��                  o  q  �!              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �!           ��                            ����                            disablePagesInFolder    
      X"      �"    a	      LOGICAL,INPUT pcPageInformation CHARACTER   enablePagesInFolder p"      �"      �"    v	      LOGICAL,INPUT pcPageInformation CHARACTER   getCallerProcedure  �"      #      P#    �	      HANDLE, getCallerWindow 0#      X#      �#    �	      HANDLE, getContainerMode    h#      �#      �#    �	      CHARACTER,  getContainerTarget  �#      �#      $    �	      CHARACTER,  getContainerTargetEvents    �#      $      L$    �	      CHARACTER,  getCurrentPage  ,$      X$      �$    �	      INTEGER,    getDisabledAddModeTabs  h$      �$      �$     �	      CHARACTER,  getDynamicSDOProcedure  �$      �$      %  !  
      CHARACTER,  getFilterSource �$      %      L%  "  '
      HANDLE, getMultiInstanceActivated   ,%      T%      �%  #  7
      LOGICAL,    getMultiInstanceSupported   p%      �%      �%  $  Q
      LOGICAL,    getNavigationSource �%      �%      &  %  k
      CHARACTER,  getNavigationSourceEvents   �%      $&      `&  &  
      CHARACTER,  getNavigationTarget @&      l&      �&  '  �
      HANDLE, getOutMessageTarget �&      �&      �&  (  �
      HANDLE, getPageNTarget  �&      �&      '  )  �
      CHARACTER,  getPageSource   �&       '      P'  *  �
      HANDLE, getPrimarySdoTarget 0'      X'      �'  +  �
      HANDLE, getReEnableDataLinks    l'      �'      �'  ,  �
      CHARACTER,  getRunDOOptions �'      �'      (  -        CHARACTER,  getRunMultiple  �'      (      D(  .        LOGICAL,    getSavedContainerMode   $(      P(      �(  /  &      CHARACTER,  getSdoForeignFields h(      �(      �(  0  <      CHARACTER,  getTopOnly  �(      �(       )  1 
 P      LOGICAL,    getUpdateSource �(      )      <)  2  [      CHARACTER,  getUpdateTarget )      H)      x)  3  k      CHARACTER,  getWaitForObject    X)      �)      �)  4  {      HANDLE, getWindowTitleViewer    �)      �)      �)  5  �      HANDLE, getStatusArea   �)       *      0*  6  �      LOGICAL,    pageNTargets    *      <*      l*  7  �      CHARACTER,INPUT phTarget HANDLE,INPUT piPageNum INTEGER setCallerObject L*      �*      �*  8  �      LOGICAL,INPUT h HANDLE  setCallerProcedure  �*      �*       +  9  �      LOGICAL,INPUT h HANDLE  setCallerWindow  +      8+      h+  :  �      LOGICAL,INPUT h HANDLE  setContainerMode    H+      �+      �+  ;  �      LOGICAL,INPUT cContainerMode CHARACTER  setContainerTarget  �+      �+      ,  <         LOGICAL,INPUT pcObject CHARACTER    setCurrentPage  �+      4,      d,  =        LOGICAL,INPUT iPage INTEGER setDisabledAddModeTabs  D,      �,      �,  >  "      LOGICAL,INPUT cDisabledAddModeTabs CHARACTER    setDynamicSDOProcedure  �,      �,       -  ?  9      LOGICAL,INPUT pcProc CHARACTER  setFilterSource  -      @-      p-  @  P      LOGICAL,INPUT phObject HANDLE   setInMessageTarget  P-      �-      �-  A  `      LOGICAL,INPUT phObject HANDLE   setMultiInstanceActivated   �-      �-       .  B  s      LOGICAL,INPUT lMultiInstanceActivated LOGICAL   setMultiInstanceSupported    .      P.      �.  C  �      LOGICAL,INPUT lMultiInstanceSupported LOGICAL   setNavigationSource l.      �.      �.  D  �      LOGICAL,INPUT pcSource CHARACTER    setNavigationSourceEvents   �.      /      P/  E  �      LOGICAL,INPUT pcEvents CHARACTER    setNavigationTarget 0/      t/      �/  F  �      LOGICAL,INPUT cTarget CHARACTER setOutMessageTarget �/      �/      �/  G  �      LOGICAL,INPUT phObject HANDLE   setPageNTarget  �/      0      L0  H  �      LOGICAL,INPUT pcObject CHARACTER    setPageSource   ,0      p0      �0  I        LOGICAL,INPUT phObject HANDLE   setPrimarySdoTarget �0      �0      �0  J        LOGICAL,INPUT hPrimarySdoTarget HANDLE  setReEnableDataLinks    �0      1      T1  K  .      LOGICAL,INPUT cReEnableDataLinks CHARACTER  setRouterTarget 41      �1      �1  L  C      LOGICAL,INPUT phObject HANDLE   setRunDOOptions �1      �1       2  M  S      LOGICAL,INPUT pcOptions CHARACTER   setRunMultiple  �1      $2      T2  N  c      LOGICAL,INPUT plMultiple LOGICAL    setSavedContainerMode   42      x2      �2  O  r      LOGICAL,INPUT cSavedContainerMode CHARACTER setSdoForeignFields �2      �2      3  P  �      LOGICAL,INPUT cSdoForeignFields CHARACTER   setTopOnly  �2      <3      h3  Q 
 �      LOGICAL,INPUT plTopOnly LOGICAL setUpdateSource H3      �3      �3  R  �      LOGICAL,INPUT pcSource CHARACTER    setUpdateTarget �3      �3      4  S  �      LOGICAL,INPUT pcTarget CHARACTER    setWaitForObject    �3      04      d4  T  �      LOGICAL,INPUT phObject HANDLE   setWindowTitleViewer    D4      �4      �4  U  �      LOGICAL,INPUT phViewer HANDLE   getObjectType   �4      �4      5  V  �      CHARACTER,  setStatusArea   �4      5      H5  W  �      LOGICAL,INPUT plStatusArea LOGICAL  applyLayout                             �5  �5      ��                  �  �  6              HN�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disableObject                                7  �6      ��                  �  �  7              (�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            enableObject                                8  �7      ��                  �  �  8              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                9  �8      ��                  �  �  $9              ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            processAction                               :  �9      ��                  �  �  (:              ���                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  @:           ��                            ����                            getAllFieldHandles  (5      �:      �:  X  	      CHARACTER,  getAllFieldNames    �:      �:      ;  Y        CHARACTER,  getCol  �:      (;      P;  Z  -      DECIMAL,    getDefaultLayout    0;      \;      �;  [  4      CHARACTER,  getDisableOnInit    p;      �;      �;  \  E      LOGICAL,    getEnabledObjFlds   �;      �;      <  ]  V      CHARACTER,  getEnabledObjHdls   �;      <      P<  ^  h      CHARACTER,  getHeight   0<      \<      �<  _ 	 z      DECIMAL,    getHideOnInit   h<      �<      �<  `  �      LOGICAL,    getLayoutOptions    �<      �<      =  a  �      CHARACTER,  getLayoutVariable   �<      =      D=  b  �      CHARACTER,  getObjectEnabled    $=      P=      �=  c  �      LOGICAL,    getObjectLayout d=      �=      �=  d  �      CHARACTER,  getRow  �=      �=      �=  e  �      DECIMAL,    getWidth    �=       >      ,>  f  �      DECIMAL,    getResizeHorizontal >      8>      l>  g  �      LOGICAL,    getResizeVertical   L>      x>      �>  h  �      LOGICAL,    setAllFieldHandles  �>      �>      �>  i        LOGICAL,INPUT pcValue CHARACTER setAllFieldNames    �>      ?      @?  j        LOGICAL,INPUT pcValue CHARACTER setDefaultLayout     ?      `?      �?  k  0      LOGICAL,INPUT pcDefault CHARACTER   setDisableOnInit    t?      �?      �?  l  A      LOGICAL,INPUT plDisable LOGICAL setHideOnInit   �?      @      <@  m  R      LOGICAL,INPUT plHide LOGICAL    setLayoutOptions    @      \@      �@  n  `      LOGICAL,INPUT pcOptions CHARACTER   setObjectLayout p@      �@      �@  o  q      LOGICAL,INPUT pcLayout CHARACTER    setResizeHorizontal �@      A      <A  p  �      LOGICAL,INPUT plResizeHorizontal LOGICAL    setResizeVertical   A      hA      �A  q  �      LOGICAL,INPUT plResizeVertical LOGICAL  getObjectTranslated |A      �A      �A  r  �      LOGICAL,    getObjectSecured    �A      B      8B  s  �      LOGICAL,    createUiEvents  B      DB      tB  t  �      LOGICAL,    bindServer                              C  �B      ��                  �  �  (C              ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               D  �C      ��                  �  �  ,D              <��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyServerObject                             E  E      ��                  �  �  4E              H��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disconnectObject                                $F  F      ��                  �  �  <F              ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              0G  G      ��                  �  �  HG              D��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            restartServerObject                             8H   H      ��                  �  �  PH              �s�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            runServerObject                             <I  $I      ��                  �  �  TI              Pt�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 lI  
         ��                            ����                            startServerObject                               lJ  TJ      ��                  �  �  �J              �t�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            unbindServer                                pK  XK      ��                  �  �  �K              x��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �K           ��                            ����                            getAppService   TB      L      8L  u  �      CHARACTER,  getASBound  L      DL      pL  v 
 �      LOGICAL,    getAsDivision   PL      |L      �L  w  �      CHARACTER,  getASHandle �L      �L      �L  x        HANDLE, getASHasStarted �L      �L      M  y        LOGICAL,    getASInfo   �L      (M      TM  z 	       CHARACTER,  getASInitializeOnRun    4M      `M      �M  {  (      LOGICAL,    getASUsePrompt  xM      �M      �M  |  =      LOGICAL,    getServerFileName   �M      �M      N  }  L      CHARACTER,  getServerOperatingMode  �M       N      XN  ~  ^      CHARACTER,  runServerProcedure  8N      dN      �N    u      HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE   setAppService   xN      �N      O  �  �      LOGICAL,INPUT pcAppService CHARACTER    setASDivision   �N      4O      dO  �  �      LOGICAL,INPUT pcDivision CHARACTER  setASHandle DO      �O      �O  �  �      LOGICAL,INPUT phASHandle HANDLE setASInfo   �O      �O       P  � 	 �      LOGICAL,INPUT pcInfo CHARACTER  setASInitializeOnRun    �O       P      XP  �  �      LOGICAL,INPUT plInitialize LOGICAL  setASUsePrompt  8P      |P      �P  �  �      LOGICAL,INPUT plFlag LOGICAL    setServerFileName   �P      �P       Q  �  �      LOGICAL,INPUT pcFileName CHARACTER  setServerOperatingMode  �P      $Q      \Q  �  �      LOGICAL,INPUT pcServerOperatingMode CHARACTER   addLink                             R   R      ��                  �  �  0R              ���                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  |R             HR  
             ��   �R             pR               �� 
                 �R  
         ��                            ����                            addMessage                              �S  xS      ��                  �  �  �S              ���                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �S             �S               ��   T             �S               ��                  T           ��                            ����                            adjustTabOrder                              U  �T      ��                  �  �  $U              ؏�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  pU             <U  
             �� 
  �U             dU  
             ��                  �U           ��                            ����                            applyEntry                              �V  lV      ��                  �  �  �V              ���                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �V           ��                            ����                            changeCursor                                �W  �W      ��                  �  �  �W              ���                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �W           ��                            ����                            createControls                              �X  �X      ��                  �  �  �X              ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �Y  �Y      ��                  �  �  �Y              X��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            displayLinks                                �Z  �Z      ��                  �  �  �Z              �_�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            editInstanceProperties                              �[  �[      ��                  �  �  \              P`�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            exitObject                              �\  �\      ��                  �  �  ]              a�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hideObject                              �]  �]      ��                  �  �  ^              �c�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �^  �^      ��                  �  �  _              �d�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            modifyListProperty                               `  �_      ��                  �  �  `              �g�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  d`             0`  
             ��   �`             X`               ��   �`             �`               ��                  �`           ��                            ����                            modifyUserLinks                             �a  �a      ��                  �  �  �a              (�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   b             �a               ��   0b             �a               �� 
                 $b  
         ��                            ����                            removeAllLinks                               c  c      ��                  �  �  8c              X�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            removeLink                               d  d      ��                  �  �  8d              ���                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �d             Pd  
             ��   �d             xd               �� 
                 �d  
         ��                            ����                            repositionObject                                �e  �e      ��                       �e              �0�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   f             �e               ��                  �e           ��                            ����                            returnFocus                             �f  �f      ��                      g              �V�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                  g  
         ��                            ����                            showMessageProcedure                                $h  h      ��                  	    <h              <#�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �h             Th               ��                  |h           ��                            ����                            toggleData                              ti  \i      ��                      �i              ���                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �i           ��                            ����                            viewObject                              �j  �j      ��                      �j              �                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            anyMessage  <Q      k      8k  � 
 U      LOGICAL,    assignLinkProperty  k      Dk      xk  �  `      LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER   fetchMessages   Xk      �k       l  �  s      CHARACTER,  getChildDataKey �k      l      <l  �  �      CHARACTER,  getContainerHandle  l      Hl      |l  �  �      HANDLE, getContainerHidden  \l      �l      �l  �  �      LOGICAL,    getContainerSource  �l      �l      �l  �  �      HANDLE, getContainerSourceEvents    �l       m      <m  �  �      CHARACTER,  getContainerType    m      Hm      |m  �  �      CHARACTER,  getDataLinksEnabled \m      �m      �m  �  �      LOGICAL,    getDataSource   �m      �m      �m  �        HANDLE, getDataSourceEvents �m       n      4n  �        CHARACTER,  getDataSourceNames  n      @n      tn  �  *      CHARACTER,  getDataTarget   Tn      �n      �n  �  =      CHARACTER,  getDataTargetEvents �n      �n      �n  �  K      CHARACTER,  getDBAware  �n      �n      (o  � 
 _      LOGICAL,    getDesignDataObject o      4o      ho  �  j      CHARACTER,  getDynamicObject    Ho      to      �o  �  ~      LOGICAL,    getInstanceProperties   �o      �o      �o  �  �      CHARACTER,  getLogicalObjectName    �o      �o      0p  �  �      CHARACTER,  getLogicalVersion   p      <p      pp  �  �      CHARACTER,  getObjectHidden Pp      |p      �p  �  �      LOGICAL,    getObjectInitialized    �p      �p      �p  �  �      LOGICAL,    getObjectName   �p      �p      ,q  �  �      CHARACTER,  getObjectPage   q      8q      hq  �  �      INTEGER,    getObjectParent Hq      tq      �q  �        HANDLE, getObjectVersion    �q      �q      �q  �        CHARACTER,  getObjectVersionNumber  �q      �q      $r  �  .      CHARACTER,  getParentDataKey    r      0r      dr  �  E      CHARACTER,  getPassThroughLinks Dr      pr      �r  �  V      CHARACTER,  getPhysicalObjectName   �r      �r      �r  �  j      CHARACTER,  getPhysicalVersion  �r      �r      (s  �  �      CHARACTER,  getPropertyDialog   s      4s      hs  �  �      CHARACTER,  getQueryObject  Hs      ts      �s  �  �      LOGICAL,    getRunAttribute �s      �s      �s  �  �      CHARACTER,  getSupportedLinks   �s      �s       t  �  �      CHARACTER,  getTranslatableProperties    t      ,t      ht  �  �      CHARACTER,  getUIBMode  Ht      tt      �t  � 
 �      CHARACTER,  getUserProperty �t      �t      �t  �  �      CHARACTER,INPUT pcPropName CHARACTER    instancePropertyList    �t      u      <u  �        CHARACTER,INPUT pcPropList CHARACTER    linkHandles u      du      �u  �         CHARACTER,INPUT pcLink CHARACTER    linkProperty    pu      �u      �u  �  ,      CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER mappedEntry �u       v      Lv  �  9      CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER  messageNumber   ,v      �v      �v  �  E      CHARACTER,INPUT piMessage INTEGER   propertyType    �v      w      <w  �  S      CHARACTER,INPUT pcPropName CHARACTER    reviewMessages  w      dw      �w  �  `      CHARACTER,  setChildDataKey tw      �w      �w  �  o      LOGICAL,INPUT cChildDataKey CHARACTER   setContainerHidden  �w      �w      ,x  �        LOGICAL,INPUT plHidden LOGICAL  setContainerSource  x      Lx      �x  �  �      LOGICAL,INPUT phObject HANDLE   setContainerSourceEvents    `x      �x      �x  �  �      LOGICAL,INPUT pcEvents CHARACTER    setDataLinksEnabled �x       y      4y  �  �      LOGICAL,INPUT lDataLinksEnabled LOGICAL setDataSource   y      \y      �y  �  �      LOGICAL,INPUT phObject HANDLE   setDataSourceEvents ly      �y      �y  �  �      LOGICAL,INPUT pcEventsList CHARACTER    setDataSourceNames  �y      z      <z  �  �      LOGICAL,INPUT pcSourceNames CHARACTER   setDataTarget   z      dz      �z  �        LOGICAL,INPUT pcTarget CHARACTER    setDataTargetEvents tz      �z      �z  �        LOGICAL,INPUT pcEvents CHARACTER    setDBAware  �z      {      <{  � 
 )      LOGICAL,INPUT lAware LOGICAL    setDesignDataObject {      \{      �{  �  4      LOGICAL,INPUT pcDataObject CHARACTER    setDynamicObject    p{      �{      �{  �  H      LOGICAL,INPUT lTemp LOGICAL setInstanceProperties   �{      |      @|  �  Y      LOGICAL,INPUT pcPropList CHARACTER  setLogicalObjectName     |      d|      �|  �  o      LOGICAL,INPUT c CHARACTER   setLogicalVersion   ||      �|      �|  �  �      LOGICAL,INPUT cVersion CHARACTER    setObjectName   �|      }      @}  �  �      LOGICAL,INPUT pcName CHARACTER  setObjectParent  }      `}      �}  �  �      LOGICAL,INPUT phParent HANDLE   setObjectVersion    p}      �}      �}  �  �      LOGICAL,INPUT cObjectVersion CHARACTER  setParentDataKey    �}      ~      @~  �  �      LOGICAL,INPUT cParentDataKey CHARACTER  setPassThroughLinks  ~      h~      �~  �  �      LOGICAL,INPUT pcLinks CHARACTER setPhysicalObjectName   |~      �~      �~  �  �      LOGICAL,INPUT cTemp CHARACTER   setPhysicalVersion  �~            H  �         LOGICAL,INPUT cVersion CHARACTER    setRunAttribute (      l      �  �        LOGICAL,INPUT cRunAttribute CHARACTER   setSupportedLinks   |      �      �  �  #      LOGICAL,INPUT pcLinkList CHARACTER  setTranslatableProperties   �      �      X�  �  5      LOGICAL,INPUT pcPropList CHARACTER  setUIBMode  8�      |�      ��  � 
 O      LOGICAL,INPUT pcMode CHARACTER  setUserProperty ��      Ȁ      ��  �  Z      LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER  showmessage ؀      8�      d�  �  j      LOGICAL,INPUT pcMessage CHARACTER   Signature   D�      ��      ��  � 	 v      CHARACTER,INPUT pcName CHARACTER    ��    )  �  p�      ,      4   ����,                ��                      ��                  *  W                  ���                       *  �        +  ��  �      <      4   ����<                (�                      ��                  ,  V                  P��                       ,  ��  (�    C  D�  ��      P      4   ����P                Ѓ                      ��                  O  Q                  ���                       O  T�         P                                  �     
                    � ߱        T�  $  S  ��  ���                           $  U  ��  ���                       8       	       	           � ߱        ��    [  Ȅ  D�      H      4   ����H                T�                      ��                  \   	                  ���                       \  ؄  ��  o   _      ,                                 ��  $   `  ��  ���                       �  @         �              � ߱        �  �   a  �      �  �   b  P      �  �   d  �      0�  �   f  8      D�  �   h  �      X�  �   j         l�  �   k  �      ��  �   l  �      ��  �   o  L      ��  �   q  �      ��  �   r  <	      І  �   t  �	      �  �   u  4
      ��  �   v  p
      �  �   w  �
       �  �   x  `      4�  �   ~  �      H�  �   �        \�  �   �  L      p�  �   �  �      ��  �   �  4      ��  �   �  �      ��  �   �  ,      ��  �   �  �      ԇ  �   �        �  �   �  �      ��  �   �        �  �   �  @      $�  �   �  �      8�  �   �  �      L�  �   �  d      `�  �   �  �      t�  �   �  �      ��  �   �        ��  �   �  T      ��  �   �  �      Ĉ  �   �        ؈  �   �  H      �  �   �  �       �  �   �  �      �  �   �  �      (�  �   �  8      <�  �   �  t      P�  �   �  �          �   �  �                      |�          �  Љ      ��                  G	  u	   �              L]�                    O   ����    e�          O   ����    R�          O   ����    ��      \     
                �       
       
       �                         � ߱        ��  $ [	  �  ���                           O   s	  ��  ��  (               �          �  �    �                                             ��                            ����                                �4      d�      ��     6     �                      V �  �                     x�    �	  ԋ  P�      4      4   ����4                `�                      ��                  �	  
                  ��                       �	  �  t�  �   �	  �      ��  �   �	        ��  �   �	  �      ��  �   �	         Č  �   �	  |      ،  �   �	  �      �  �   �	  l       �  �   �	  �      �  �   �	  d      (�  �   �	  �      <�  �   �	  T      P�  �   �	  �      d�  �   �	  L          �   �	  �      P�    '
  ��  �      8      4   ����8                 �                      ��                  (
  �
                  �P�                       (
  ��  4�  �   *
  �      H�  �   +
        \�  �   ,
  �      p�  �   -
  �      ��  �   .
  p       ��  �   /
  �       ��  �   0
  `!      ��  �   1
  �!      Ԏ  �   2
  H"      �  �   3
  �"      ��  �   4
  8#      �  �   5
  �#      $�  �   6
   $      8�  �   7
  �$      L�  �   8
  %      `�  �   9
  �%      t�  �   :
  &      ��  �   ;
  �&      ��  �   <
  '      ��  �   =
  �'      ď  �   >
   (      ؏  �   ?
  |(      �  �   @
  �(       �  �   A
  t)      �  �   B
  �)      (�  �   C
  l*      <�  �   D
  �*          �   E
  d+      l�    �
  l�  �      �+      4   �����+                ��                      ��                  �
  t                  �R�                       �
  |�  �  �   �
  ,,       �  �   �
  �,      4�  �   �
  $-      H�  �   �
  �-      \�  �   �
  .      p�  �   �
  �.      ��  �   �
  �.      ��  �   �
  0/      ��  �   �
  �/      ��  �   �
  �/      ԑ  �   �
  0      �  �   �
  �0      ��  �   �
  1      �  �   �
  �1      $�  �   �
  �1      8�  �   �
  h2      L�  �   �
  �2      `�  �   �
  X3      t�  �   �
  �3      ��  �   �
  4      ��  �   �
  �4      ��  �   �
  �4      Ē  �   �
  l5      ؒ  �   �
  �5      �  �   �
  �5       �  �   �
  `6      �  �   �
  �6      (�  �   �
  �6      <�  �   �
  7      P�  �   �
  P7      d�  �   �
  �7      x�  �   �
  �7      ��  �   �
  8      ��  �   �
  x8      ��  �   �
  �8      ȓ  �   �
  �8      ܓ  �   �
  ,9      �  �   �
  h9      �  �   �
  �9      �  �   �
  �9      ,�  �   �
  :      @�  �   �
  �:      T�  �   �
  ;      h�  �   �
  x;      |�  �   �
  �;      ��  �   �
  h<      ��  �   �
  �<      ��  �   �
  `=      ̔  �   �
  �=      ��  �   �
  X>      ��  �   �
  �>      �  �   �
  ?      �  �   �
  �?      0�  �      �?      D�  �     @      X�  �     @@          �     �@      ĕ  $  �  ��  ���                       A     
                    � ߱        \�    �  ��  �      (A      4   ����(A      /   �  �     ,�                          3   ����8A            L�                      3   ����XA  ��    �  x�  ��  ��  tA      4   ����tA  	              �                      ��             	     �  H                  �s�                       �  ��  �  �   �  �A      p�  $  �  D�  ���                        B     
                    � ߱        ��  �   �   B      ܗ  $   �  ��  ���                       HB  @         4B              � ߱        ��  $  �  �  ���                       �B                         � ߱        C     
                �C       
       
       �D  @        
 �D              � ߱        (�  V   �  4�  ���                        �D                     E                     XE                         � ߱        ��  $  �  Ę  ���                       F     
                �F       
       
       �G  @        
 �G              � ߱        H�  V     T�  ���                        �G     
                lH       
       
       �I  @        
 |I              � ߱            V   ,  �  ���                        
              ��                      ��             
     J  �                  @u�                       J  t�  �I     
                DJ       
       
       �K  @        
 TK          �K  @        
 �K          XL  @        
 L          �L  @        
 xL              � ߱            V   _  �  ���                        adm-clone-props \�  ԛ              �     7     `                          \  �#                     start-super-proc    �  @�  �           �     8                                  �#                     H�    �  ̜  ܜ      DP      4   ����DP      /      �     �                          3   ����TP            8�                      3   ����tP  ��  $    t�  ���                       �P                         � ߱        \�    *  ��  8�  ؞  �P      4   �����P                ��                      ��                  +  /                  ���                       +  ̝  �P                     �P                     �P                         � ߱            $  ,  H�  ���                             0  ��  0�      Q      4   ����Q  $Q                         � ߱            $  1  �  ���                       X�    8  x�  ��  ��  8Q      4   ����8Q      $  9  ��  ���                       XQ                         � ߱            �   V  lQ      �Q     
                (R       
       
       xS  @        
 8S              � ߱        ��  V   j  ��  ���                        ��  �   �  �S      0�      ��  Ġ      �S      4   �����S      /      �      �                          3   �����S             �                      3   �����S  �  $  $  \�  ���                       T                         � ߱        <T     
                �T       
       
       V  @        
 �U              � ߱        �  V   .  ��  ���                        ��    �  4�  ��      V      4   ����V                ��                      ��                  �  �                  ���                       �  D�      g   �  آ         ����                           ��          p�  X�      ��                  �      ��              ��                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  ̣     ܣ  <V                      3   ����$V  �     
   ��                      3   ����HV         
   ,�                      3   ����PV    ��                              ��        '	                  ����                                        �              9      <�                      g                                �  g   �  �          ��	��                           إ          ��  ��      ��                  �  �  ��              ���                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  �     �  tV                      3   ����XV            4�                      3   ����|V    ��                              ��        '	                  ����                                        $�              :      D�                      g                               �  g   �  �          ��	��                           �          ��  ��      ��                  �  �  ȧ              $��                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  �     �  �V                      3   �����V            <�                      3   �����V    ��                              ��        '	                  ����                                        ,�              ;      L�                      g                               h�    �  $�  ��      �V      4   �����V                ��                      ��                  �  �                  �i�                       �  4�  �  /   �  ܩ     �                          3   �����V            �                      3   ����W  �  /  �  H�     X�  DW                      3   ����$W  ��     
   x�                      3   ����LW  ��        ��                      3   ����TW  �        ت                      3   ����hW            �                      3   �����W  @�    �  4�  D�      �W      4   �����W      /  �  p�     ��  8X                      3   ����X  ��     
   ��                      3   ����@X  �        Ы                      3   ����HX  �         �                      3   ����\X            0�                      3   �����X        �  \�  l�      �X      4   �����X      /  �  ��     ��  �X                      3   �����X  ج     
   Ȭ                      3   �����X  �        ��                      3   ����Y  8�        (�                      3   ����Y            X�                      3   ����4Y  (�    �  ��   �      XY      4   ����XY                �                      ��                  �  �                  h��                       �  ��      g   �  (�         ��̯        hY                  �          ��  ��      ��                  �      خ              ̶�                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  �     ,�  �Y                      3   ����tY  \�     
   L�                      3   �����Y         
   |�                      3   �����Y    ��                            ����                                        <�              <      ��                      g                               ��     �  �Y                                     �Y     
                8Z       
       
       �[  @        
 H[              � ߱        P�  V   f  \�  ���                        �[     
                \       
       
       h]  @        
 (]              � ߱        |�  V   �  �  ���                         �    �  ��  ��      |]      4   ����|]      $   �  Ա  ���                       �]  @         �]              � ߱        Գ  g   �  �         ��x�        �]  ��x�        �]                  ��          Ĳ  ��      ��                  �  �  ܲ              p��                    O   ����    e�          O   ����    R�          O   ����    ��            �  �   �      ^      4   ����^      O  �  ������  ^    ��                            ����                                        @�              =      8�                      g                               ��  g   �  �         �6$�         0^                  ��          ��  l�      ��                  �  �  ��              ��                    O   ����    e�          O   ����    R�          O   ����    ��      ̴    �  <^  }          O  �  ������  P^    ��                            ����                                         �              >      �                      g                               4�  g   �  ��         �"ض                           `�          0�  �      ��                  �  �  H�              ��                    O   ����    e�          O   ����    R�          O   ����    ��            �  d^  }        ��                              ��        '	                  ����                                        ��              ?      x�                      g                               ��  g   �  L�         �"d�                           \�          �  ̷      ��                  �  %  ��              t��                    O   ����    e�          O   ����    R�          O   ����    ��                                                       � ߱        ��  $   �  �   �                       ȸ  r                    �^  �^  �^  ��  x^  5  �^  �^  �      �  ��      �^      4   �����^      O     ��  ��  �^   �  �     _      \�  /    L�                               3   ����$_  ��  /     ��                                 3   ����D_  ��  /    Ĺ     Թ  �_                      3   ����`_  �     
   ��                      3   �����_  4�        $�                      3   �����_            T�  d�                  3   �����_      $     ��  ���                                                   � ߱        �  /    �     ��  �_                      3   �����_  (�     
   �                      3   �����_  X�        H�                      3   �����_            x�  ��                  3   ����`      $     ��  ���                                                   � ߱        �  �   "  `  �  �   #  `          	  $  8�                                        3   ����<`               �          ļ  ؼ   \ |�                            Archivo_Excel                   
                 $   <   L          $   <   L    Archivo_Excel �       ��                              ��        '	                  ����                            T�          `�      H�     @     �                      g    �                                @  ܽ  X�      H`      4   ����H`                ̾                      ��                  @  l                  ��                       @  �  X`  @                     �`  @         p`          �`  @         �`              � ߱        ��  $   A  h�  ���                       ��  g   G  �         �n��      }                      ؿ          ��  ��      ��                  H  L  ��              ,��                    O   ����    e�          O   ����    R�          O   ����    ��      �  /  I  �                                 3   �����`        J  0�  @�      �`      4   �����`      O  K  ������  a    ��                            ����                                        $�              A      X�                      g                               ��  g   Q  �         �!l�         a                   �          ��  ��      ��                  Q  S  ��              ���                    O   ����    e�          O   ����    R�          O   ����    ��      (a  @                         � ߱            $  R  ��  ���                         ��                            ����                                         �              B      ,�                      g                               �  /   V  ��                                 3   ����0a        ]   �  ��      La      4   ����La                �                      ��                  ]  j                  h��                       ]  0�                X�          @�  (�      ��                 a  h                  ��                       a  ��      O   a    ��          O   a    ��      ��  /   e  ��                                 3   ����da        f  ��  ��      �a      4   �����a      k   g  ��              }       n        �   adm-create-objects  d�  ��                      C      �                               �%                     Carga-Temporal  �  d�          :  8:    D    �:  /                      �:  (                     disable_UI  t�  ��                      E      <                              #(  
                   enable_UI   ��  8�                      F      �                              .(  	                   exitObject  D�  ��                      G      �                               8(  
                   initializeObject    ��  �                      H      �                              C(                     items-pesos �  x�  �       t      '   I     �                          �  �(                     ue-sectores ��  ��  �       �      *   J     �                          �  %)                      ����   � �����  �        ��  8   ����)   ��  8   ����)   ��  8   ����&   ��  8   ����&   ��  8   ����%   ��  8   ����%   ��  % 
 ��  8   ����$   �  8   ����$   �  $  �  8   ����#   ,�  8   ����#   <�  #  D�  8   ����"   T�  8   ����"   d�  "  l�  8   ����!   |�  8   ����!   ��  !  ��  8   ����   ��  8   ����   ��    ��  8   ����    ��  8   ����           ��  8   ����   ��  8   ����   ��  8   ����   �  8   ����       8   ����       8   ����       ,�  8�      toggleData  ,INPUT plEnabled LOGICAL    �  d�  |�      showMessageProcedure    ,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL  T�  ��  ��      returnFocus ,INPUT hTarget HANDLE   ��  ��  �      repositionObject    ,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL    ��  D�  P�      removeLink  ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE 4�  ��  ��      removeAllLinks  ,   ��  ��  ��      modifyUserLinks ,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE ��  0�  D�      modifyListProperty  ,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER     �  ��  ��      hideObject  ,   ��  ��  ��      editInstanceProperties  ,   ��  �  �      displayLinks    ,   ��  ,�  <�      createControls  ,   �  P�  `�      changeCursor    ,INPUT pcCursor CHARACTER   @�  ��  ��      applyEntry  ,INPUT pcField CHARACTER    |�  ��  ��      adjustTabOrder  ,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER ��  ,�  8�      addMessage  ,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER �  ��  ��      addLink ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE ��  ��  ��      unbindServer    ,INPUT pcMode CHARACTER ��  $�  8�      startServerObject   ,   �  L�  \�      runServerObject ,INPUT phAppService HANDLE  <�  ��  ��      restartServerObject ,   x�  ��  ��      initializeServerObject  ,   ��  ��  ��      disconnectObject    ,   ��  �  �      destroyServerObject ,   ��  ,�  8�      bindServer  ,   �  L�  \�      processAction   ,INPUT pcAction CHARACTER   <�  ��  ��      enableObject    ,   x�  ��  ��      disableObject   ,   ��  ��  ��      applyLayout ,   ��  ��  ��      viewPage    ,INPUT piPageNum INTEGER    ��  (�  4�      viewObject  ,   �  H�  P�      toolbar ,INPUT pcValue CHARACTER    8�  |�  ��      selectPage  ,INPUT piPageNum INTEGER    l�  ��  ��      removePageNTarget   ,INPUT phTarget HANDLE,INPUT piPage INTEGER ��  �  �      passThrough ,INPUT pcLinkName CHARACTER,INPUT pcArgument CHARACTER  ��  X�  d�      notifyPage  ,INPUT pcProc CHARACTER H�  ��  ��      initPages   ,INPUT pcPageList CHARACTER |�  ��  ��      initializeVisualContainer   ,   ��  ��   �      hidePage    ,INPUT piPageNum INTEGER    ��  ,�  <�      destroyObject   ,   �  P�  \�      deletePage  ,INPUT piPageNum INTEGER    @�  ��  ��      createObjects   ,   x�  ��  ��      constructObject ,INPUT pcProcName CHARACTER,INPUT phParent HANDLE,INPUT pcPropList CHARACTER,OUTPUT phObject HANDLE ��  0�  <�      confirmExit ,INPUT-OUTPUT plCancel LOGICAL   �  l�  x�      changePage  ,   \�  ��  ��      assignPageProperty  ,INPUT pcProp CHARACTER,INPUT pcValue CHARACTER      � 
"     
 �%     adecomm/as-utils.w 
"   
   �    }        �
"     
   %                  �     }        �G� ?	   �G%              � C	     %       	  %       P       %        %       %        %       %               %               %               %              %              %              %               %              
�        
"   
 �
�    
"   
 �
"   
 H    �        �     �            
"   
   �        D         �     }        �%              
"   
 �
"   
 H    �        �     �        �    
"   
   �        �         �     }        �%              � 
"    
 �%              � �  �         X      $              
�    � �   �     
"   
 �                      
�            � �   H
"    
 H
�H T   %              �     }        �GG %              � 
"   
   P �L 
�H T   %              �     }        �GG %              
"   
   �        �    7%               
"   
 ��           �    1� �  
 �� �   �%               o%   o           � �    �
"   
 ��           D    1� �   �� �   �%               o%   o           � �   �
"   
 ��           �    1� �  
 �� �   �%               o%   o           � �   �
"   
 ��           ,    1� �   �� �   �%               o%   o           � �   �
"   
 ��           �    1� �   �� �   �%               o%   o           � �   �
"   
 ��               1� 
   ��    �%               o%   o           %               
"   
 ��          �    1�    �� .     
"   
 ��           �    1� 5   �� �   �%               o%   o           � H  e �
"   
 ��           @    1� �   �� �   �%               o%   o           � �  [ �
"   
 ��           �    1�    ��    �%               o%   o           %               
"   
 ��           0	    1� )   ��    �%               o%   o           %               
"   
 ��           �	    1� ;   ��    �%               o%   o           %              
"   
 ��          (
    1� H   ��      
"   
 ��           d
    1� W  
 ��    �%               o%   o           %               
"   
 ��           �
    1� b   �� �   �%               o%   o           � �    �
"   
 ��          T    1� j   �� .     
"   
 ��           �    1� z   �� �   �%               o%   o           � �  t �
"   
 ��              1�   
 �� .     
"   
 ��           @    1�    �� �   �%               o%   o           � !  � �
"   
 ��           �    1� �   �� �   �%               o%   o           � �    �
"   
 ��           (    1� �  
 �� �   �%               o%   o           %               
"   
 ��           �    1� �   ��    �%               o%   o           %               
"   
 ��                1� �   �� �   �%               o%   o           � �    �
"   
 ��           �    1� �   �� �   �%               o%   o           o%   o           
"   
 ��               1� �  
 �� �   �%               o%   o           � �    �
"   
 ��           �    1�    ��   	 �%               o%   o           � #  / �
"   
 ��          �    1� S   ��   	   
"   
 ��           4    1� e   ��   	 �o%   o           o%   o           � �    �
"   
 ��          �    1� x   ��   	   
"   
 ��           �    1� �   ��   	 �o%   o           o%   o           � �    �
"   
 ��          X    1� �   ��      
"   
 ��          �    1� �   ��   	   
"   
 ��          �    1� �   ��   	   
"   
 ��              1� �   ��   	   
"   
 ��           H    1� �   ��    �o%   o           o%   o           %              
"   
 ��          �    1� �   ��   	   
"   
 ��               1� �  
 �� �     
"   
 ��          <    1� �   ��   	   
"   
 ��          x    1�    ��   	   
"   
 ��          �    1� !   ��   	   
"   
 ��          �    1� 6   ��   	   
"   
 ��          ,    1� E  	 ��   	   
"   
 ��          h    1� O   ��   	   
"   
 ��          �    1� b   ��   	   
"   
 ��           �    1� y   �� �   �%               o%   o           o%   o           
�H T   %              �     }        �GG %              
"   
   
"   
 �
"   
   
"   
 H(�  L ( l       �        �    �� �   � P   �        �    �@    
� @  , 
�       �    �� �     p�               �L
�    %              � 8      �    � $         � �          
�    � �     
"   
 �� @  , 
�       �    �� �  
 �p�               �L"      P �L 
�H T   %              �     }        �GG %              
"   
 ��           �    1� �  
 �� �   �%               o%   o           � �    �
"   
 ��           �    1� �  
 �� �   �%               o%   o           o%   o           
"   
 ��           x    1� �   �� .   �%               o%   o           o%   o           
"   
 ��           �    1� �   ��    �%               o%   o           %               
"   
 ��           p    1� �   ��    �%               o%   o           %               
"   
 ��           �    1� �   �� �   �%               o%   o           � �    �
"   
 ��           `    1� �   ��    �%               o%   o           %              
"   
 ��           �    1�    ��    �%               o%   o           o%   o           
"   
 ��           X    1�    �� �   �%               o%   o           o%   o           
"   
 ��           �    1�    	 �� �   �%               o%   o           � �    �
"   
 ��           H    1� *   �� �   �%               o%   o           o%   o           
"   
 ��           �    1� >   �� �   �%               o%   o           o%   o           
"   
 ��           @    1� M   ��    �%               o%   o           %               
"   
 ��           �    1� ]   ��    �%               o%   o           o%   o           P �L 
�H T   %              �     }        �GG %              
"   
 ��           �    1� i   ��   	 �%               o%   o           � �    �
"   
 ��                1� v   ��   	 �%               o%   o           � �    �
"   
 ��           t    1� �   ��    �%               o%   o           %               
"   
 ��           �    1� �   ��   	 �%               o%   o           � �    �
"   
 ��           d     1� �   ��   	 �%               o%   o           � �    �
"   
 ��           �     1� �   ��    �%               o%   o           %               
"   
 ��           T!    1� �   ��   	 �%               o%   o           � �    �
"   
 ��           �!    1� �   ��   	 �%               o%   o           � �    �
"   
 ��           <"    1� �   ��   	 �%               o%   o           � �    �
"   
 ��           �"    1� �   ��   	 �%               o%   o           o%   o           
"   
 ��           ,#    1� �   ��   	 �%               o%   o           � �    �
"   
 ��           �#    1�    ��   	 �%               o%   o           � �    �
"   
 ��           $    1�   	 �� �   �%               o%   o           %               
"   
 ��           �$    1�    �� �   �%               o%   o           %               
"   
 ��           %    1� (   ��    �%               o%   o           o%   o           
"   
 ��           �%    1� 9   ��    �%               o%   o           o%   o           
"   
 ��           &    1� H   ��    �%               o%   o           %               
"   
 ��           �&    1� V   ��    �%               o%   o           %               
"   
 ��           �&    1� g   ��    �%               o%   o           %               
"   
 ��           x'    1� |   �� �   �%               o%   o           %       
       
"   
 ��           �'    1� �   �� �   �%               o%   o           o%   o           
"   
 ��           p(    1� �   �� �   �%               o%   o           %              
"   
 ��           �(    1� �   �� �   �%               o%   o           o%   o           
"   
 ��           h)    1� �   �� �   �%               o%   o           %              
"   
 ��           �)    1� �   �� �   �%               o%   o           o%   o           
"   
 ��           `*    1� �   �� �   �%               o%   o           %              
"   
 ��           �*    1� �   �� �   �%               o%   o           o%   o           
"   
 ��           X+    1� �   ��   	 �%               o%   o           � �    �P �L 
�H T   %              �     }        �GG %              
"   
 ��            ,    1� �   �� �   �%               o%   o           %               
"   
 ��           �,    1� �   �� �   �%               o%   o           o%   o           
"   
 ��           -    1�    �� �   �%               o%   o           � �    �
"   
 ��           �-    1�    �� �   �%               o%   o           � .  - �
"   
 ��            .    1� \   �� �   �%               o%   o           � �    �
"   
 ��           t.    1� s   �� �   �%               o%   o           � �   �
"   
 ��          �.    1� �   �� .     
"   
 ��           $/    1� �   �� �   �%               o%   o           � �    �
"   
 ��          �/    1� �  
 �� .     
"   
 ��          �/    1� �   �� .     
"   
 ��           0    1� �   ��   	 �%               o%   o           � �    �
"   
 ��           �0    1� �   �� �   �%               o%   o           � �    �
"   
 ��           �0    1� �   �� .   �%               o%   o           o%   o           
"   
 ��           t1    1� 
    �� �   �%               o%   o           �    ! �
"   
 ��           �1    1� ?    �� �   �%               o%   o           � �    �
"   
 ��           \2    1� L    �� �   �%               o%   o           � _    �
"   
 ��           �2    1� n   	 �� �   �%               o%   o           o%   o           
"   
 ��           L3    1� x    ��    �%               o%   o           %               
"   
 ��          �3    1� �    �� .     
"   
 ��           4    1� �    �� �   �%               o%   o           � �    �
"   
 ��           x4    1� �    ��   	 �%               o%   o           � �    �
"   
 ��           �4    1� �    ��   	 �%               o%   o           � �    �
"   
 ��          `5    1� �    �� .     
"   
 ��          �5    1� �    ��   	   
"   
 ��           �5    1� �    ��    �o%   o           o%   o           %               
"   
 ��          T6    1� !   ��      
"   
 ��          �6    1� %!   ��   	   
"   
 ��          �6    1� 3!   ��   	   
"   
 ��          7    1� F!   ��   	   
"   
 ��          D7    1� W!   ��   	   
"   
 ��          �7    1� h!   ��   	   
"   
 ��          �7    1� y!   �� .     
"   
 ��           �7    1� �!   �� �   �%               o%   o           � �!  4 �
"   
 ��          l8    1� �!   �� .     
"   
 ��          �8    1� �!   �� .     
"   
 ��          �8    1� �!   �� .     
"   
 ��           9    1�  "   ��   	   
"   
 ��          \9    1� "   ��   	   
"   
 ��          �9    1� &"   ��   	   
"   
 ��          �9    1� 8"   ��      
"   
 ��           :    1� E"   ��   	 �%               o%   o           � �    �
"   
 ��           �:    1� S"   ��   	 �%               o%   o           � �    �
"   
 ��           �:    1� _"   ��   	 �%               o%   o           � �    �
"   
 ��           l;    1� t"   ��   	 �%               o%   o           � �    �
"   
 ��           �;    1� �"   ��    �%               o%   o           %               
"   
 ��           \<    1� �"   ��    �%               o%   o           o%   o           
"   
 ��           �<    1� �"   ��    �%               o%   o           %               
"   
 ��           T=    1� �"   ��    �%               o%   o           %               
"   
 ��           �=    1� �"   ��    �%               o%   o           o%   o           
"   
 ��           L>    1� �"   ��    �%               o%   o           %               
"   
 ��          �>    1� �"   ��   	   
"   
 ��           ?    1� �"   ��    �%               o%   o           %              
"   
 ��          �?    1� #   ��   	   
"   
 ��          �?    1� #   ��   	   
"   
 ��          �?    1� (#  
 ��   	   
"   
 ��           4@    1� 3#   ��   	 �%               o%   o           � �"   �
"   
 ��           �@    1� E#   ��   	 �%               o%   o           � �    �
"   
    "  	  �%     start-super-proc n�%     adm2/smart.p �HP �L 
�H T   %              �     }        �GG %              
"   
   �       �A    6� �     
"   
   
�        �A    8
"   
   �        B    ��     }        �G 4              
"   
 ߱G %              G %              %p e `   LogicalObjectName,PhysicalObjectName,DynamicObject,RunAttribute,HideOnInit,DisableOnInit,ObjectLayout H
�H T   %              �     }        �GG %              
"   
 H
"   
 �
"   
 H
"   
   (�  L ( l       �        \C    �� �   � P   �        hC    �@    
� @  , 
�       tC    �� �   Hp�               �L
�    %              � 8      �C    � $         � �          
�    � �   H
"   
 �p� @  , 
�       �D    �� 5   �p�               �L"    , �   � �#   �� �#   ��     }        �A      |    "      � �#   �%              (<   \ (    |    �     }        �A� �#   �A"    �    "    H"    �  < "    H"    �(    |    �     }        �A� �#   �A"    �
�H T   %              �     }        �GG %              
"   
 H
"   
 �
"   
 H
"   
   (�  L ( l       �        dF    �� �   � P   �        pF    �@    
� @  , 
�       |F    �� �   Hp�               �L
�    %              � 8      �F    � $         � �          
�    � �   H
"   
 �p� @  , 
�       �G    �� �  
 �p�               �L"    , 
�H T   %              �     }        �GG %              
"   
 H
"   
 �
"   
 H
"   
   (�  L ( l       �        <H    �� �   � P   �        HH    �@    
� @  , 
�       TH    �� �   Hp�               �L
�    %              � 8      `H    � $         � �          
�    � �   H
"   
 �p� @  , 
�       pI    ��    �p�               �L
"   
 , 
�H T   %              �     }        �GG %              
"   
   
"   
 �
"   
   
"   
   (�  L ( l       �        J    �� �   � P   �         J    �@    
� @  , 
�       ,J    �� �     p�               �L
�    %              � 8      8J    � $         � �          
�    � �     
"   
 �p� @  , 
�       HK    �� �  
 �p�               �L%     SmartWindow 
"   
   p� @  , 
�       �K    �� �     p�               �L%      WINDOW  
"   
  p� @  , 
�       L    �� �    p�               �L%               
"   
  p� @  , 
�       lL    �� e    p�               �L(        � �      � �      � �      �     }        �A
�H T   %              �     }        �GG %              
"   
 � (   � 
"   
 H    �        LM    �� �   �
"   
   � 8      �M    � $         � �          
�    � �   H
"   
   �        �M    �
"   
   �       N    /
"   
   
"   
   �       <N    6� �     
"   
   
�        hN    8
"   
   �        �N    �
"   
   �       �N    �
"   
   p�    � �#   �
�    �     }        �G 4              
"   
 ߱G %              G %              
�     }        �
"   
    (   � 
"   
 H    �        lO    �A"    �A
"   
   
�        �O    �@ � 
"   
 �"      �       }        �
"   
 �%              %                "  	  �%     start-super-proc n�%     adm2/appserver.p ���    � 0$     
�    �     }        �%               %      Server  - �     }        �    "    �� �    �%                   "    �� �    �%      NONE    p�,  8         $     "    �        � J$   H
�    
�H T   %              �     }        �GG %              
"   
 H
"   
 �
"   
 H
"   
   (�  L ( l       �        �Q    �� �   � P   �        R    �@    
� @  , 
�       R    �� �   Hp�               �L
�    %              � 8      R    � $         � �          
�    � �   H
"   
 �p� @  , 
�       ,S    �� *   �p�               �L"    , p�,  8         $     "    �        � X$   H
�     "  	  �%     start-super-proc m�%     adm2/visual.p H�   � �     � |$     � ~$  #   
�H T   %              �     }        �GG %              
"   
 H
"   
 �
"   
 H
"   
   (�  L ( l       �        �T    �� �   � P   �        �T    �@    
� @  , 
�       �T    �� �   Hp�               �L
�    %              � 8      �T    � $         � �          
�    � �   H
"   
 �p� @  , 
�       �U    �� �   �p�               �L"    , � 
" 	   
 �%     contextHelp 
" 	   
   
�    
�    %     processAction   
�    %     CTRL-PAGE-UP �H%     processAction   
�    %     CTRL-PAGE-DOWN  "  	  �%     start-super-proc l�%     adm2/containr.p %     modifyListProperty 
�    
�    %      Add     %      ContainerSourceEvents �%      initializeDataObjects �0 0   A    �    � �$   �
�    � �$   �A    �    � �$     
�    � %   �%     modifyListProperty 
�    
�    %      Add     %      ContainerSourceEvents �%     buildDataRequest ent0 A    �    � �$   �
�    � #%   �%     modifyListProperty 
�    
�    %      Add     %     SupportedLinks %      ContainerToolbar-Target � 
" 	   
 �
"   
 �%     contextHelp 
" 	   
   
�    
�    %               
�H T   %              �     }        �GG %              
"   
 H
"   
 �
"   
 H
"   
 �(�  L ( l       �        Z    �� �   � P   �        Z    �@    
� @  , 
�        Z    �� �   Hp�               �L
�    %              � 8      ,Z    � $         � �   H     
�    � �   �
"   
 �p� @  , 
�       <[    �� �    �p�               �L
�             �G
�H T   %              �     }        �GG %              
"   
 H
"   
 �
"   
 H
"   
 H(�  L ( l       �        �[    �� �   � P   �        �[    �@    
� @  , 
�        \    �� �   Hp�               �L
�    %              � 8      \    � $         � �   H     
�    � �   H
"   
 �p� @  , 
�       ]    �� �"   �p�               �L%              (        �     }        �G� ?	   �G� 
"   
 H
"   
   �        �]    �%              
"   
 �
"   
 ��     }        �%               
"   
 �%      CLOSE   %               %      CLOSE   "      � �%     � �%     %              � �%     � �%         "    �%               %               �    }        �� �%     %     lib\Tools-to-excel %     Carga-Temporal  %     pi-crea-archivo-csv 
"   
   
�     
        �G"      "      %     pi-crea-archivo-xls 
"   
   
�     
        �G"      "      
"   
   �    }        �� �%      � �%     � 
"   
 �
"   
 �
"   
 H�        d`    %%              
�     }        �
"   
   %     destroyObject       �     }        �    �  � �%  	   %               
"   
 �
�    %     createObjects    �     }        �%     initializeObject l� �     }        �� A&         %              %                   "      %                  "      �     "      �     "      T    "      "      � �&   �"     �"     �"    �"    �"    �&    &    &    &    &    &    �    p    L    0        %              %              %                  "      &        "      &        "      &    "      "      &    &    &    &        %              %              "  O    ((       "      %              � �%      z     "    H%     ue-sectores "    �"    �"    �"      "      "      %               %               %     items-pesos "  	    "  
    "       "  
    � �%      "  	    T   %              "  >    � �&     "(< @       �    "  >  ߱� �&     %              "� @    T   %              "  >    � �&     "%   "           "      � �&     � �&     � �&     "       � �&     "      %     ue-sectores "    �"    �"    �"      "      "      "      %              "      %              "      %              "      %                  "      "      %              � �&         "    �"    �� �&  
   P (   (         "    �%                   "      "    H    "    �%               � �&     � (   X     ( (       "    �%                   "      %                   "      "    H    "      %               � �&     X     ( (       "    �%                   "      %                   "      "    H� �&     X     ( (       "    �%                   "      %                   "      "    H� '     X     ( (       "    �%                   "      %                   "      "    H� '         "      � ''     � )'     �            B           "      � >'     "      "      "  !    "      "      &    &    &    &    &    &    &    &    L    0        %              %              %              %              "  >    "      "      "  X    "  Y    "      "      "  7    "  =    "  	    "  B    "  O    ""  P "   "  C    "      "     �"  K  �"  %  �&    &    &    &    &    &    0        %              %              %              *    � ['   �"  8  �&    &    &    &        %              %              * !   " !         "      � ^'     "          "  K    � b'     "     �"    �&    &    &    &        %              %              * "   "     �" "   �&    &    &    &        %              %              *     4    3    %              4    3    %              4    3    %              &    &    &    &    &    &    0        %              %              %              * #   " #     � �'   �"     �"    �"    �&    &    &    &    &    &    &    &    L    0        %              %              %              %              "" $  " �"     �" $   �" $   �&    &    &    &    &    &    0        %              %              %              " %     � �'   �� �&   �"     �"    �"    �&    &    &    &    &    &    p    T    8        %                  " &     &    %              %                  " & 4    &    " &   �" &     � (   �"     �" &   �" &   �&    &    &    &    &    &    &    &    L    0        %              %              %              %              * $   "     �" $   �" $   �&    &    &    &    &    &    0        %              %              %              * $   " %     �            B 4               � (   ߱"      � >'     "      �            B� �%      (        �     }        �G� ?	   �G� 
"   
 H
"   
   �     }        �
�    
"   
 H"    �"    H"      
"   
 �
"   
   %      CLOSE   %                   +  %              +  %      SUPER   %               %               "      "      "      "      &    &    &    &    &    &    T    8        %                  " ( !   &    %              %              " (     " (     &    &    &    &        %              %                   " '     %              ( (       " ) +    %                  " ) +    %                     " '         " (     " ) +    " '     " '     "     �" *   �" *   �&    &    &    &    &    &    0        %              %              %                   " *     %              <      %                  " & V    � �%           " * 	    %              <      %                  " & T    � �%           " * 
    %              <      %                  " & a    � �%           " *     %              " *     " * 	    " * 
    " *                     �           �   l       ��                 W  {  �               �w�                    O   ����    e�          O   ����    R�          O   ����    ��        $  f  �   ���                        M     
                    � ߱              g  (  �      XM      4   ����XM                �                      ��                  h  z                  ��                       h  8  �  �  i  �M            k  �  `      �M      4   �����M                p                      ��                  l  y                  ���                       l  �  �  o   m      ,                                 �  �   n  N      �  �   o  HN      $  $  p  �  ���                       tN     
                    � ߱        8  �   q  �N      L  �   r  �N      `  �   u  �N          $   x  �  ���                       O  @         �N              � ߱                     T          ,  @   T �            
             
             
             
                 $   4   D          $   4   D   ����        ��                            ����                                            �           �   l       ��                 �  �  �               ؁�                    O   ����    e�          O   ����    R�          O   ����    ��      �#                      �          �  $  �    ���                       XO     
                    � ߱                  �  �                      ��                   �  �                  H�                     �  4      4   ����xO      $  �  �  ���                       �O     
                    � ߱        �    �  4  D      �O      4   �����O      /  �  p                               3   �����O  �  �   �  �O          O   �  ��  ��  0P                               , �                          
                               �      ��                            ����                                                        �   l       ��                  w  ~  �               ,��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                                            �           �   l       ���;               �  A  �               ���                    O   ����    e�          O   ����    R�          O   ����    ��      �   �   �     �  $  �  �   ���                       �a                         � ߱          �      �  X                      ��        0         �  �                  ���      (b     L     �        $  �  �  ���                       �a                         � ߱        H  $  �    ���                       �a                         � ߱            4   ���� b    $  �  �  ���                       <b                         � ߱              ,      d  �;      4        ��                  �  �  L              ���                       �  �  �  X  �       ��                            7   ����          ��               �b    �            �                  6   �        H   ��           �b    �            �                                                        \b   hb   tb   �b   �b   �b                   �  �           �b  �b  �b           �b  �b  �b                      d   |        �  0       ��$                           A   ����           ��               �c    �            �                  6   �         �   ��         �  �c    �            �                          *                              �c   �c                     �           �c  �c           �c  �c         �            �   �        O   ����  e�          O   ����  R�          O   ����  ��      �  $  �  �  ���                       �c                         � ߱          $  �  �  ���                        d                         � ߱        �	  /   �  @     P                          3   ����Pd  �        p                      3   ����hd  �        �                      3   ����td  8        �  �                  3   �����d      $   �    ���                                                   � ߱        �        X  h                  3   �����d      $   �  �  ���                                                   � ߱        H	        �  �                  3   �����d      $   �  	  ���                                                   � ߱                  h	  x	                  3   �����d      $   �  �	  ���                                                   � ߱        (
  $  �  �	  ���                       �d       	       	           � ߱        �
  $  �  T
  ���                       �d       
       
           � ߱        �  /   �  �
     �
                          3   �����d  D        �
  �
                  3   �����d      $   �    ���                                 	       	           � ߱                  d  t                  3   �����d      $   �  �  ���                                 
       
           � ߱        �  9   �     �  �   �        �  �       
  0 0 0      [ [ [      W W W      ) + +      ] ] ]      & ) )                              � � �      � � �       ! !                      X X X      �        8 8 8      � � �      K K K      $ ' '              u t t      n m m      V V V      d c c      p o o      _ _ _      ` ` `      s r r      t s s      o n n      f e e      { z z      � � �       
 
      J J J      | { {      Z Z Z      } | |      z y y      \ \ \      * , ,      6 6 6      G G G      7 7 7     " P P P                      R R R      ; ; ;      � � �      9 9 9       $ $      - - -      N N N                                M M M      
        S S S      H H H      < < <      � � �                      L L L              ,                       3 3 3      ~ } }                      v u u      w v v      = = =      > > >      ? ? ?      @ @ @      A A A      B B B      C C C      D D D      E E E      ^ ^ ^      # % %      + & &      � � �      � � �      � � �      � � �      � � �      � � �      T T T      ( * *       	 	      1 1 1      Y Y Y              % ( (      '        q p p      r q q              a a a      e d d      b b b      x w w     4 4 4      � � �      � � �        # #               ~ ~      � � �      g f f      h g g      i h h      j i i      k j j      l k k      	        I I I      m l l   
  2 2 2       " "      � � �      � � �      � � �      � � �      � � �      � � �      � � �      � � �      � � �      � � �      � � �      � � �      � � �              . . .      !       / / /      Q Q Q      5 5 5      F F F      "        O O O      : : :              y x x      U U U   e       =       =       e      B       B        e       >       >       ,e      C       C           � ߱        �  $  �  4  ���                       8  $  �    ���                       8e       O       O           � ߱        �  $  �  d  ���                       le     "  P       P           � ߱        |  p   �  f  �      �  �  0     f  f  (f                @                      ��                  �  �                  D��                       �  �      $  �  l  ���                       4f       =       =           � ߱                 @f                $                      ��                  �  �                  ���                       �  �      $  �  P  ���                       Lf       =       =           � ߱        �  /   �  �     �                          3   ����Xf  �        �                      3   ����pf                                3   ����|f  �        8  H                  3   �����f      $   �  t  ���                                                   � ߱        (        �  �                  3   �����f      $   �  �  ���                                                   � ߱        �        H  X                  3   �����f      $   �  �  ���                                                   � ߱                  �  �                  3   �����f      $   �    ���                                                   � ߱        �f      0   �f  0       �f      0   �f  0       �f      0   g  0       g      0   $g  0       8g      0   Xg  0           � ߱           $  �  8  ���                       X  $  �  ,  ���                       lg       >       >           � ߱        �    �  t  �      xg      4   ����xg      $  �  �  ���                       �g       >       >           � ߱        `    �  �        �g      4   �����g      $  �  4  ���                       $h       >       >           � ߱        �    �  |  �      0h      4   ����0h      $  �  �  ���                       �h       >       >           � ߱        h    �           �h      4   �����h      $  �  <  ���                       li       >       >           � ߱        �    �  �  �      xi      4   ����xi      $  �  �  ���                       �i       >       >           � ߱        p    �          j      4   ����j      $  �  D  ���                       �j       >       >           � ߱        �    �  �  �      �j      4   �����j      $  �  �  ���                       �j       >       >           � ߱            $   �     ���                       �j  @         �j              � ߱        �  �   �           �      �!  �;      �!  �!      ��                  �  >  �!              ���                �9     �  \  $     T       ��                            7   ����          ��                     �            �                  6   �        �   ��                    �            �                                                                                                       @            �   �        P   �        ��$                           A   ����          ��               tk    �            �                   6   �        4!   ��         !  tk    �            �                           *                              k   k   k   (k                   �!  �!           4k  Dk  Tk  dk           <k  Lk  \k  lk         �            P!   l!        O   ����  e�          O   ����  R�          O   ����  ��      �#  9   �     �k                     �k                     �k                      l                     l       	       	       l       
       
       $l                     0l                     <l                     Hl                     Tl                     `l                     ll     "                xl                     �l                         � ߱        �#  $  �   "  ���                       �$  A  �        H$   ��         0$  �l                                         �l   �l   �l                 �$  �$           �l  �l  �l           �l  �l  �l         �            d$   |$    p-    �  �$  L%  �'  0m      4   ����0m                \%                      ��                  �                    ���                       �  �$  $&  A          ! �%   ��         �%  pm                                         8m   Dm                   &  &           Pm  `m           Xm  hm         �            �%   �%    �&      @&  |&      �m      4   �����m  �m                         � ߱            $    P&  ���                               �&   '      �m      4   �����m  �m                         � ߱            $    �&  ���                                     �'                      ��                                      ���                         ,'          �'  @(      �m      4   �����m                P(                      ��                                      d��                         �'  )  A         " �(   ��         �(  8n                                          n   n                   )  �(           n  (n            n  0n         �            �(   �(          
  4)  �)      hn      4   ����hn  	              �)                      ��                  
                    pS�                       
  D)  �*  A           $*   ��         *  �n                                         pn   |n                   t*  h*           �n  �n           �n  �n         �            @*   T*            �*   +      �n      4   �����n  
              0+                      ��                                      8T�                         �*  ,  A         # �+   ��        	 �+  |o                                         �n   o   (o                 �+  �+           Lo  \o  lo           To  do  to         �            �+   �+             ,  �,      �o      4   �����o                �,                      ��                                      �.�                         0,  �o                         � ߱            $    �,  ���                             �-      2          �1  �1      ��                    &  �1              4/�                �2       -  @0  �-  �-  �/                                7   ����    $     	 ��          
     Lp    ��1          L.                  6          $ �.  	 ��        
 p.  Lp    ��1          L.                                                        �o   �o   �o    p                   �.  �.      	     p  p  ,p  <p      	     p  $p  4p  Dp                      �.   �.    p4    ,/      x/          -          $                                                                                                                                                           J             $0    ��                                                          �p                      0              l0  �0       ��$                           A   ����   %     
 ��               q    �            1                  6         % H1  
 ��         01  q    �            1                          *                              �p   �p   �p                 �1  �1      
     �p  �p  q      
     �p  �p  q         �            d1   |1            O   ����  e�          O   ����  R�          O   ����  ��      \2  $  $  02  ���                       `q                         � ߱            O   %  �� ��            �2      ,6          �5  �5      ��                  )  <  �5              (9�                T9     )  t2      3  l3  85                               7   ����    &      ��               �q    ��5          �3                  6   )       & 4   ��         �3  �q    ��5          �3                                                        lq   xq   �q   �q   �q                 \4  P4           �q  �q  �q           �q  �q  �q         �             4   84          �4      �4         t2          &                                                                                                                                                                   J   )         �5    ��                                                          lr                      �5                O   ����  e�          O   ����  R�          O   ����  ��      xr                         � ߱        X6  $  /   6  ���                       <7  B  1       $ �6   ��         �6  �r                                         �r   �r   �r   �r                   (7  7           �r  �r  �r  �r           �r  �r  �r  �r                      �6    7    <9    5  X7  �7      \s      4   ����\s                �7                      ��                  5  :                  L��                       5  h7  �8  A  6       % L8  
 ��         48  �s                                         ds   ps   |s                 �8  �8      
     �s  �s  �s      
     �s  �s  �s         �            h8   �8          9  �8  �8      t      4   ����t      $  9  9  ���                       t                         � ߱            O   ;  �� ��          $   =  �9  ���                       ,t  @         t              � ߱            $   ?  �9  ���                       �t  @         tt              � ߱                      l:                                               �:          �:  �:   , �:                      Todos                                                        ��                             ��                              ��        '	                   ��                             ��                             ��                            ����                            �;  % 
 �;  $  �;  #  �;  "  �;  !  �;        =   >                =   �                     �           �   l       ��                  G  T  �               ���                    O   ����    e�          O   ����    R�          O   ����    ��           Q  �   �       �t      4   �����t      n   R     �          �t        S    ,      �t      4   �����t      �   S  �t    ��                            ����                                            �           �   l       ��                  Z  j  �               <��                    O   ����    e�          O   ����    R�          O   ����    ��      u  �           u  �           u  �              � ߱        p  Z   d  �    �        �t                  �               �              �              �              � ߱        �  h   f      �        ,u                  
   i  �� �             8u    ��                              ��        '	                  ����                                            �           �   l       ��                  p  z  �               P��                    O   ����    e�          O   ����    R�          O   ����    ��      �     w  Du  }          O   x  ��  ��  Xu    ��                            ����                                            �           �   l       ��                  �  �  �               (��                    O   ����    e�          O   ����    R�          O   ����    ��        $  �  �   ���                       lu                         � ߱        d  $  �  8  ���                       �u                         � ߱            /   �  �                                3   �����u    ��                            ����                                                    �   l   $  ��                 �  �  �               ���                    O   ����    e�          O   ����    R�          O   ����    ��      T(   '    �              �          [(   '                   �          \  $  �  0  ���                       �u      '                   � ߱           $  �  �  ���                       �u      '                   � ߱              0      \          ,        ��                  �  �  D              H��                �     �  �  �  \  �       ��                            7   ����    (      ��               ,v    �            �                  6   �       ( @   ��            ,v    �            �                                                        �u   �u   �u   �u                   �  �           �u  v  v           v  v  $v                      \   t        �  (       ��$                           A   ����    )      ��               �v    �            x                  6   �       ) �   ��         �  �v    �            x                          *                              �v   �v                      �           �v  �v           �v  �v         �            �   �        O   ����  e�          O   ����  R�          O   ����  ��      �  $  �  �  ���                       w      '                   � ߱              �  �  L      ,w      4   ����,w                \                      ��                  �  �                  �z�                       �  �      $  �  �  ���                       �w      '                   � ߱        �  8  �  (     $  �  �  ���                       �w      '                   � ߱            $  �  H  ���                       �w      '                   � ߱                    '  �                                          ' (   ��                             ��                            ����                                8   �  (       8   �  (                             �   l       ��                 �  �  �               �{�                    O   ����    e�          O   ����    R�          O   ����    ��      �(   *    �              �          �(   *                 �          �(   *    8                      �(   *    `             ,         �(   *    �             T         �(   *                   |                      �          �  �      ��                  �  �  �              �g�                4     �  �      L  �       ��                            7   ����    &      ��               $x    �            �                  6   �       & (   ��           $x    �            �                                                        �w   �w   �w                 �  t       	    �w  x  x           �w  x  x                      D   \        O   ����  e�          O   ����  R�          O   ����  ��      4  $  �    ���                       px      *                   � ߱        4    �  P  �      �x      4   �����x                �                      ��                  �  �                  $,�                       �  `      $  �    ���                       �x      * 	       	           � ߱        4    �  P  �       y      4   ���� y                �                      ��                  �  �                  �,�                       �  `      $  �    ���                       @y      * 
       
           � ߱              �  P  �      hy      4   ����hy                �                      ��                  �  �                  �,�                       �  `      $  �    ���                       �y      *                   � ߱        �  $  �  `  ���                       �y      *                   � ߱        �  $  �  �  ���                       �y      *                   � ߱        <  $  �    ���                       �y      *                   � ߱            $  �  h  ���                       �y      *                   � ߱                    *  �                                              *     ��                             ��                            ����                               �   d d     L   ��@A  � �                                               '	                                                                         d     D                                                                 P   4;�d                                                           �)  G   
 X  4;xd                                                         	     -	  
    P   �;Xd                                                           �)  G   
 X  �;xd                                                        	     -	  
    `  D;                                                        �        $                \  ,~��                                 �                 �)      8        @      `  ~                                                          �        $                  \  x~��             d                   �                 �)      �        H     
 X  �
��d                                                        	     8	       D                                                                    TXS appSrvUtils ORDENES Pedidos al Credito CodCia CodDoc NroPed FchPed fchven UsrDscto CodCli NomCli DirCli RucCli ordcmp Hora TpoPed CodAlm CodMon TpoCmb usuario Glosa Observa ImpBrt ImpExo PorIgv ImpDto ImpTot ImpIgv CodVen ImpIsc ImpVta ImpFle FlgSit FmaPgo CodDiv TipVta PorDto FlgEst LugEnt LugEnt2 CodTrans NroRef Cmpbnte NCmpbnte Atencion FaxCli FlgIgv TpoLic Ubigeo AcuBon NroCard TipBon Importe Porcent UsrAprobacion FchAprobacion FchEnt CodPos FlgEnv UsrSac FecSac HorSac Libre_c01 Libre_c02 Libre_c03 Libre_c04 Libre_c05 Libre_d01 Libre_d02 Libre_f01 Libre_f02 UsrChq FchChq HorChq Sede DivDes CodRef ImpDto2 GlosaImpDto2 FlgImpOD UsrImpOD FchImpOD UsrAct FecAct HorAct MotReposicion VtaPuntual CrossDocking AlmacenXD CodOrigen NroOrigen DT AlmacenDT EmpaqEspec Cliente_Recoge Lista_de_Precios CustomerPurchaseOrder CustomerRequest OrderType Period Currency PayForm DeliveryDate Region1 Region1Name Region2 Region2Name Region3 Region3Name TelephoneContactReceptor ContactReceptorName DeliveryAddress CustomerLabel OfficeCustomer OfficeCustomerName CustomerStockDepo CustomerStockDepoName ConsolidateInvoiceCustomer InvoiceCustomerGroup Items Peso Volumen Embalaje_Especial DeliveryGroup DNICli e-mail ImpPercepcion PorPercepcion CodPais CodDept CodProv CodDist ReferenceAddress IDCustomer FlagMigracion MigFecha MigHora MigUsuario TotalValorVentaNetoOpGravadas TotalValorVentaNetoOpGratuitas TotalTributosOpeGratuitas TotalIGV TotalImpuestos TotalValorVenta TotalPrecioVenta DescuentosGlobales PorcentajeDsctoGlobal MontoBaseDescuentoGlobal TotalValorVentaNetoOpNoGravada TotalDocumentoAnticipo MontoBaseDsctoGlobalAnticipo PorcentajeDsctoGlobalAnticipo TotalDsctoGlobalesAnticipo MontoBaseICBPER TotalMontoICBPER TotalValorVentaNetoOpExoneradas TotalVenta x-Faccpedi FacCPedi s-codcia s-coddiv ASSIGNFOCUSEDWIDGET ASSIGNWIDGETVALUE ASSIGNWIDGETVALUELIST BLANKWIDGET CLEARWIDGET DISABLERADIOBUTTON DISABLEWIDGET ENABLERADIOBUTTON ENABLEWIDGET FORMATTEDWIDGETVALUE FORMATTEDWIDGETVALUELIST HIDEWIDGET HIGHLIGHTWIDGET RESETWIDGETVALUE TOGGLEWIDGET VIEWWIDGET WIDGETHANDLE WIDGETLONGCHARVALUE WIDGETISBLANK WIDGETISFOCUSED WIDGETISMODIFIED WIDGETISTRUE WIDGETVALUE WIDGETVALUELIST Detalle Situacion SitPedido NroHPK SitHPK CodPed Origen ImpRef nomPos wWin BtnDone img/exit.ico BUTTON-15 img/excel.bmp FILL-IN-Mensaje txtDesde txtHasta fMain 99/99/9999 X(256) GUI REPORTE DE ESTADOS DE PEDIDOS DISABLEPAGESINFOLDER ENABLEPAGESINFOLDER GETCALLERPROCEDURE GETCALLERWINDOW GETCONTAINERMODE GETCONTAINERTARGET GETCONTAINERTARGETEVENTS GETCURRENTPAGE GETDISABLEDADDMODETABS GETDYNAMICSDOPROCEDURE GETFILTERSOURCE GETMULTIINSTANCEACTIVATED GETMULTIINSTANCESUPPORTED GETNAVIGATIONSOURCE GETNAVIGATIONSOURCEEVENTS GETNAVIGATIONTARGET GETOUTMESSAGETARGET GETPAGENTARGET GETPAGESOURCE GETPRIMARYSDOTARGET GETREENABLEDATALINKS GETRUNDOOPTIONS GETRUNMULTIPLE GETSAVEDCONTAINERMODE GETSDOFOREIGNFIELDS GETTOPONLY GETUPDATESOURCE GETUPDATETARGET GETWAITFOROBJECT GETWINDOWTITLEVIEWER GETSTATUSAREA PAGENTARGETS SETCALLEROBJECT SETCALLERPROCEDURE SETCALLERWINDOW SETCONTAINERMODE SETCONTAINERTARGET SETCURRENTPAGE SETDISABLEDADDMODETABS SETDYNAMICSDOPROCEDURE SETFILTERSOURCE SETINMESSAGETARGET SETMULTIINSTANCEACTIVATED SETMULTIINSTANCESUPPORTED SETNAVIGATIONSOURCE SETNAVIGATIONSOURCEEVENTS SETNAVIGATIONTARGET SETOUTMESSAGETARGET SETPAGENTARGET SETPAGESOURCE SETPRIMARYSDOTARGET SETREENABLEDATALINKS SETROUTERTARGET SETRUNDOOPTIONS SETRUNMULTIPLE SETSAVEDCONTAINERMODE SETSDOFOREIGNFIELDS SETTOPONLY SETUPDATESOURCE SETUPDATETARGET SETWAITFOROBJECT SETWINDOWTITLEVIEWER GETOBJECTTYPE SETSTATUSAREA GETALLFIELDHANDLES GETALLFIELDNAMES GETCOL GETDEFAULTLAYOUT GETDISABLEONINIT GETENABLEDOBJFLDS GETENABLEDOBJHDLS GETHEIGHT GETHIDEONINIT GETLAYOUTOPTIONS GETLAYOUTVARIABLE GETOBJECTENABLED GETOBJECTLAYOUT GETROW GETWIDTH GETRESIZEHORIZONTAL GETRESIZEVERTICAL SETALLFIELDHANDLES SETALLFIELDNAMES SETDEFAULTLAYOUT SETDISABLEONINIT SETHIDEONINIT SETLAYOUTOPTIONS SETOBJECTLAYOUT SETRESIZEHORIZONTAL SETRESIZEVERTICAL GETOBJECTTRANSLATED GETOBJECTSECURED CREATEUIEVENTS GETAPPSERVICE GETASBOUND GETASDIVISION GETASHANDLE GETASHASSTARTED GETASINFO GETASINITIALIZEONRUN GETASUSEPROMPT GETSERVERFILENAME GETSERVEROPERATINGMODE RUNSERVERPROCEDURE SETAPPSERVICE SETASDIVISION SETASHANDLE SETASINFO SETASINITIALIZEONRUN SETASUSEPROMPT SETSERVERFILENAME SETSERVEROPERATINGMODE gshAstraAppserver gshSessionManager gshRIManager gshSecurityManager gshProfileManager gshRepositoryManager gshTranslationManager gshWebManager gscSessionId gsdSessionObj gshFinManager gshGenManager gshAgnManager gsdTempUniqueID gsdUserObj gsdRenderTypeObj gsdSessionScopeObj ghProp ghADMProps ghADMPropsBuf glADMLoadFromRepos glADMOk ANYMESSAGE ASSIGNLINKPROPERTY FETCHMESSAGES GETCHILDDATAKEY GETCONTAINERHANDLE GETCONTAINERHIDDEN GETCONTAINERSOURCE GETCONTAINERSOURCEEVENTS GETCONTAINERTYPE GETDATALINKSENABLED GETDATASOURCE GETDATASOURCEEVENTS GETDATASOURCENAMES GETDATATARGET GETDATATARGETEVENTS GETDBAWARE GETDESIGNDATAOBJECT GETDYNAMICOBJECT GETINSTANCEPROPERTIES GETLOGICALOBJECTNAME GETLOGICALVERSION GETOBJECTHIDDEN GETOBJECTINITIALIZED GETOBJECTNAME GETOBJECTPAGE GETOBJECTPARENT GETOBJECTVERSION GETOBJECTVERSIONNUMBER GETPARENTDATAKEY GETPASSTHROUGHLINKS GETPHYSICALOBJECTNAME GETPHYSICALVERSION GETPROPERTYDIALOG GETQUERYOBJECT GETRUNATTRIBUTE GETSUPPORTEDLINKS GETTRANSLATABLEPROPERTIES GETUIBMODE GETUSERPROPERTY INSTANCEPROPERTYLIST LINKHANDLES LINKPROPERTY MAPPEDENTRY MESSAGENUMBER PROPERTYTYPE REVIEWMESSAGES SETCHILDDATAKEY SETCONTAINERHIDDEN SETCONTAINERSOURCE SETCONTAINERSOURCEEVENTS SETDATALINKSENABLED SETDATASOURCE SETDATASOURCEEVENTS SETDATASOURCENAMES SETDATATARGET SETDATATARGETEVENTS SETDBAWARE SETDESIGNDATAOBJECT SETDYNAMICOBJECT SETINSTANCEPROPERTIES SETLOGICALOBJECTNAME SETLOGICALVERSION SETOBJECTNAME SETOBJECTPARENT SETOBJECTVERSION SETPARENTDATAKEY SETPASSTHROUGHLINKS SETPHYSICALOBJECTNAME SETPHYSICALVERSION SETRUNATTRIBUTE SETSUPPORTEDLINKS SETTRANSLATABLEPROPERTIES SETUIBMODE SETUSERPROPERTY SHOWMESSAGE SIGNATURE , prepareInstance ObjectName CHAR  ObjectVersion ADM2.2 ObjectType SmartWindow ContainerType WINDOW PropertyDialog adm2/support/visuald.w QueryObject LOGICAL ContainerHandle HANDLE InstanceProperties LogicalObjectName,PhysicalObjectName,DynamicObject,RunAttribute,HideOnInit,DisableOnInit,ObjectLayout SupportedLinks Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source ContainerHidden ObjectInitialized ObjectHidden ObjectsCreated HideOnInit UIBMode ContainerSource ContainerSourceEvents initializeObject,hideObject,viewObject,destroyObject,enableObject,confirmExit,confirmCancel,confirmOk,isUpdateActive DataSource DataSourceEvents dataAvailable,queryPosition,updateState,deleteComplete,fetchDataSet,confirmContinue,confirmCommit,confirmUndo,assignMaxGuess,isUpdatePending TranslatableProperties ObjectPage INT DBAware DesignDataObject DataSourceNames DataTarget DataTargetEvents CHARACTER updateState,rowObjectState,fetchBatch,LinkState LogicalObjectName PhysicalObjectName LogicalVersion PhysicalVersion DynamicObject RunAttribute ChildDataKey ParentDataKey DataLinksEnabled InactiveLinks InstanceId DECIMAL SuperProcedure SuperProcedureMode SuperProcedureHandle LayoutPosition ClassName RenderingProcedure ThinRenderingProcedure Label cType ADMProps Target WHERE Target = WIDGET-H(" ") AppService ASDivision ASHandle ASHasConnected ASHasStarted ASInfo ASInitializeOnRun ASUsePrompt BindSignature BindScope ServerOperatingMode ServerFileName ServerFirstCall NeedContext ObjectLayout LayoutOptions ObjectEnabled LayoutVariable DefaultLayout DisableOnInit EnabledObjFlds EnabledObjHdls FieldSecurity SecuredTokens AllFieldHandles AllFieldNames MinHeight MinWidth ResizeHorizontal ResizeVertical ObjectSecured ObjectTranslated PopupButtonsInFields ColorInfoBG INTEGER ColorInfoFG ColorWarnBG ColorWarnFG ColorErrorBG ColorErrorFG BGColor FGColor FieldPopupMapping CurrentPage PendingPage ContainerTarget ContainerTargetEvents exitObject,okObject,cancelObject,updateActive ContainerToolbarSource ContainerToolbarSourceEvents toolbar,okObject,cancelObject OutMessageTarget PageNTarget PageSource FilterSource UpdateSource UpdateTarget CommitSource CommitSourceEvents commitTransaction,undoTransaction CommitTarget CommitTargetEvents rowObjectState StartPage RunMultiple WaitForObject DynamicSDOProcedure adm2/dyndata.w RunDOOptions InitialPageList WindowFrameHandle Page0LayoutManager MultiInstanceSupported MultiInstanceActivated ContainerMode SavedContainerMode SdoForeignFields NavigationSource NavigationTarget PrimarySdoTarget NavigationSourceEvents fetchFirst,fetchNext,fetchPrev,fetchLast,startFilter CallerWindow CallerProcedure CallerObject DisabledAddModeTabs ReEnableDataLinks WindowTitleViewer UpdateActive InstanceNames ClientNames ContainedDataObjects ContainedAppServices DataContainer HasDbAwareObjects HasDynamicProxy HideOnClose HideChildContainersOnClose HasObjectMenu RequiredPages RemoveMenuOnHide ProcessList PageLayoutInfo PageTokens DataContainerName WidgetIDFileName ghContainer adm2/smart.p cObjectName iStart / \ . hReposBuffer hPropTable hBuffer hTable deleteProperties ADM-CLONE-PROPS pcProcName hProc START-SUPER-PROC cAppService cASDivision cServerOperatingMode adm2/appserver.p getAppService Server NONE setASDivision setAppService cFields adm2/visual.p   txtDesde txtHasta BUTTON-15 BtnDone CTRL-PAGE-UP CTRL-PAGE-DOWN adm2/containr.p Add initializeDataObjects getSupportedLinks data-target data-source buildDataRequest containertoolbar-target ContainerToolbar-Target CLOSE c-csv-file c-xls-file Archivo_Excel rpta Libro de Excel *.xlsx .xlsx Guardar como GENERAL  Proceso Concluido iStartPage ADM-ERROR ADM-CREATE-OBJECTS lOrdenCompra lUserImpresion lSectores lSecImp lSecAsig lSecDev lSecxAsig lCodDoc k O/D,O/M,OTR x-items x-peso x-docproc pSoloImpresos s-CodDoc Todos Pedidos al Credito A GN-DIVI DIVISIONES | O/D O/M ODC OTR SIN EMPEZAR COMPLETADO PARCIALMENTE IMPRESOS SOLO IMPRESOS AVANCE PARCIAL SOLO ASIGNADOS ASIGNADO PARCIAL C FACTURADO/G.REMISION   almtabla Tablas de Almacen CP PED R/A Almacen TabDistr Tabla de Distritos LogTrkDocs Tracking de Documentos TRCKPED TabTrkDocs Maestro Tracking de Documentos VtaCDocu Cabecera de COT PED O/D HPK TRCKHPK Detalle: CARGA-TEMPORAL DISABLE_UI ENABLE_UI EXITOBJECT INITIALIZEOBJECT pItems pPeso lPeso lItems b-facdpedi FacDPedi Almmmatg Cat�logo de Materiales ITEMS-PESOS pCodDoc pNroDoc pSectores pSecImp pSecAsig pSecDev nSectores nSectoresImp nSectoresAsig nSectoresReto nSectoresSinAsig UE-SECTORES llave01 llave02 llave03 llave04 llave05 llave06 Llave07 llave08 Llave09 Llave10 Llave11 Llave12 Llave13 Llave14 Llave15 default Desde Hasta Button 15 &Done IDX01 tabl01 alm01 Llave01 Idx00 Idx01 Matg01 $  �6  T  D=      % �8   ��      0         pcProp      ��      P         pcProp      ��      p         plCancel    �   ��      �         pcProcName  �   ��      �        
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
 hTarget X  ��      L        pcMessage       ��      p        pcMessage       ��      �        plEnabled             �     cType       �     6   �          �                  getObjectType   [	  s	  u	  ,          
   hReposBuffer    L        @  
   hPropTable  h        `  
   hBuffer           |  
   hTable  �  �     7             �                  adm-clone-props f  g  h  i  k  l  m  n  o  p  q  r  u  x  y  z  {              
   hProc             <        pcProcName  �  �  	   8     $      x                  start-super-proc    �  �  �  �  �  �  �  �  �  H  �     9                                   �  �  	     :                                   �  �  �  L	     ;                                   �  �  	  �	     <                                   �  T	  �	     =                                   �  �  �	  �	     >                                   �  �  �  �	  ,
     ?                                   �  �  T
        H
     c-csv-file  t
        h
     c-xls-file  �
        �
     rpta              �
  
   hProc   �	  �
     @   4
                              �                "  #  $  %  �
  <     A                                   I  J  K  L    |     B                                   R  S  L  �     C               �                  adm-create-objects  ~  �        �     lOrdenCompra                 lUserImpresion  4        (     lSectores   P        H     lSecImp p        d     lSecAsig    �        �     lSecDev �        �     lSecxAsig   �       �     lCodDoc �       �     k   �     	   �     x-items      
        x-peso  8        ,     x-docproc   \        L     pSoloImpresos             p     s-CodDoc    �  �  N   D   �          �                  Carga-Temporal  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �                 
                        $  %  &  )  /  1  5  6  9  :  ;  <  =  >  ?  A  |  0     E               $                  disable_UI  Q  R  S  T  �  |     F               p                  enable_UI   d  f  i  j  @  �     G               �                  exitObject  w  x  z  �       H                                 initializeObject    �  �  �  �  D  '      <     lPeso       '      X     lItems  �  '      x        pItems      '      �        pPeso        (  C  �  b-facdpedi  �  �     I   (  `  �  �                  items-pesos �  �  �  �  �  �  �  �  �  �  �  �  H  *      <     nSectores   l  *   	   \     nSectoresImp    �  *   
   �     nSectoresAsig   �  *      �     nSectoresReto       *      �     nSectoresSinAsig    �  *      �        pCodDoc   *              pNroDoc @  *      4        pSectores   `  *      X        pSecImp �  *      x        pSecAsig        *      �        pSecDev �  �     J   (  �      �                  ue-sectores �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  @*       �#       )                      0"  d  l  �   ORDENES �         �         �         �         �         �         �         �         �         �         �                                                        (         0         8         @         H         P         X         `         h         p         x         �         �         �         �         �         �         �         �         �         �         �         �         �         �         �                                            $  
      ,         4  
      <        D        L         \         l         t         |         �         �         �         �         �         �         �         �         �         �         �         �                                              (         0         8         @         P         \         h      "   t         |         �         �         �         �         �         �         �         �         �         �         �                           4         D         P         X         d         l         |         �         �         �         �         �         �         �         �         �                           ,         @         X         t         �         �         �         �         �         �         �         �         �         �                                                  ,          8          H          T          \          h          �          �          �          �          �          �          !         !         0!         L!         l!         �!         �!         �!         �!         �!         "         $"         CodCia  CodDoc  NroPed  FchPed  fchven  UsrDscto    CodCli  NomCli  DirCli  RucCli  ordcmp  Hora    TpoPed  CodAlm  CodMon  TpoCmb  usuario Glosa   Observa ImpBrt  ImpExo  PorIgv  ImpDto  ImpTot  ImpIgv  CodVen  ImpIsc  ImpVta  ImpFle  FlgSit  FmaPgo  CodDiv  TipVta  PorDto  FlgEst  LugEnt  LugEnt2 CodTrans    NroRef  Cmpbnte NCmpbnte    Atencion    FaxCli  FlgIgv  TpoLic  Ubigeo  AcuBon  NroCard TipBon  Importe Porcent UsrAprobacion   FchAprobacion   FchEnt  CodPos  FlgEnv  UsrSac  FecSac  HorSac  Libre_c01   Libre_c02   Libre_c03   Libre_c04   Libre_c05   Libre_d01   Libre_d02   Libre_f01   Libre_f02   UsrChq  FchChq  HorChq  Sede    DivDes  CodRef  ImpDto2 GlosaImpDto2    FlgImpOD    UsrImpOD    FchImpOD    UsrAct  FecAct  HorAct  MotReposicion   VtaPuntual  CrossDocking    AlmacenXD   CodOrigen   NroOrigen   DT  AlmacenDT   EmpaqEspec  Cliente_Recoge  Lista_de_Precios    CustomerPurchaseOrder   CustomerRequest OrderType   Period  Currency    PayForm DeliveryDate    Region1 Region1Name Region2 Region2Name Region3 Region3Name TelephoneContactReceptor    ContactReceptorName DeliveryAddress CustomerLabel   OfficeCustomer  OfficeCustomerName  CustomerStockDepo   CustomerStockDepoName   ConsolidateInvoiceCustomer  InvoiceCustomerGroup    Items   Peso    Volumen Embalaje_Especial   DeliveryGroup   DNICli  e-mail  ImpPercepcion   PorPercepcion   CodPais CodDept CodProv CodDist ReferenceAddress    IDCustomer  FlagMigracion   MigFecha    MigHora MigUsuario  TotalValorVentaNetoOpGravadas   TotalValorVentaNetoOpGratuitas  TotalTributosOpeGratuitas   TotalIGV    TotalImpuestos  TotalValorVenta TotalPrecioVenta    DescuentosGlobales  PorcentajeDsctoGlobal   MontoBaseDescuentoGlobal    TotalValorVentaNetoOpNoGravada  TotalDocumentoAnticipo  MontoBaseDsctoGlobalAnticipo    PorcentajeDsctoGlobalAnticipo   TotalDsctoGlobalesAnticipo  MontoBaseICBPER TotalMontoICBPER    TotalValorVentaNetoOpExoneradas TotalVenta      @"  H"     Detalle 8#         D#         P#         X#         `#         h#         p#         |#         �#         �#         �#         �#         �#         �#         �#         �#      "   �#         �#         �#         �#         Situacion   SitPedido   NroHPK  SitHPK  CodPed  NroPed  CodOrigen   NroOrigen   FchPed  Hora    FchEnt  Origen  NomCli  Peso    UsrImpOD    FchImpOD    Items   Glosa   ImpRef  nomPos  $          $  
   appSrvUtils 0$        $$     s-codcia    P$        D$     s-coddiv    l$       d$  
   wWin    �$       �$     FILL-IN-Mensaje �$       �$     txtDesde    �$       �$     txtHasta    �$        �$  
   gshAstraAppserver    %  	 	     %  
   gshSessionManager   D%  
 
     4%  
   gshRIManager    l%        X%  
   gshSecurityManager  �%        �%  
   gshProfileManager   �%        �%  
   gshRepositoryManager    �%        �%  
   gshTranslationManager   &         &  
   gshWebManager   4&        $&     gscSessionId    X&        H&     gsdSessionObj   |&        l&  
   gshFinManager   �&        �&  
   gshGenManager   �&        �&  
   gshAgnManager   �&        �&     gsdTempUniqueID '        �&     gsdUserObj  0'        '     gsdRenderTypeObj    X'        D'     gsdSessionScopeObj  t'       l'  
   ghProp  �'       �'  
   ghADMProps  �'       �'  
   ghADMPropsBuf   �'    	   �'     glADMLoadFromRepos  �'    
   �'     glADMOk (       (  
   ghContainer <(       0(     cObjectName X(       P(     iStart  x(       l(     cAppService �(       �(     cASDivision �(       �(     cServerOperatingMode    �(       �(     cFields          �(     iStartPage  )    L  )  ORDENES 4)     C  ()  x-Faccpedi  L)    L  D)  Detalle h)       \)  FacCPedi    �)        x)  GN-DIVI �)   !    �)  almtabla    �)   "    �)  Almacen �)   #    �)  TabDistr    �)   $    �)  LogTrkDocs  *   %    �)  TabTrkDocs  $*    &    *  VtaCDocu        ! )    4*  Almmmatg             B   b  c  t  �  �  �  �  �  �  �  )  *  +  ,  C  O  P  Q  S  U  V  W  [  \  _  `  a  b  d  f  h  j  k  l  o  q  r  t  u  v  w  x  ~  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �   	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  
  '
  (
  *
  +
  ,
  -
  .
  /
  0
  1
  2
  3
  4
  5
  6
  7
  8
  9
  :
  ;
  <
  =
  >
  ?
  @
  A
  B
  C
  D
  E
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
           t  �  �  �  �  �  �  �  �  �  �  �  �    ,  H  J  _  �  �       *  +  ,  /  0  1  8  9  V  j  �       $  .  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  f  �  �  �  �  �  �  �  @  A  G  Q  V  ]  a  e  f  g  h  j  l      H� $ C:\Progress\OpenEdge\src\adm2\windowmn.i T.  f!  C:\Progress\OpenEdge\src\adm2\containr.i �.  � # %C:\Progress\OpenEdge\src\adm2\custom\containrcustom.i    �.  ��  C:\Progress\OpenEdge\src\adm2\visual.i    /  # " %C:\Progress\OpenEdge\src\adm2\custom\visualcustom.i  4/  �<  C:\Progress\OpenEdge\src\adm2\appserver.i    t/  �� ! %C:\Progress\OpenEdge\src\adm2\custom\appservercustom.i   �/  I�  C:\Progress\OpenEdge\src\adm2\smart.i    �/  Ds   C:\Progress\OpenEdge\gui\fn  $0  tw  %C:\Progress\OpenEdge\src\adm2\custom\smartcustom.i   L0  Q.  C:\Progress\OpenEdge\gui\set �0  ��  C:\Progress\OpenEdge\src\adm2\cntnprop.i �0  ��  %C:\Progress\OpenEdge\src\adm2\custom\cntnpropcustom.i    �0  P  %C:\Progress\OpenEdge\src\adm2\custom\cntnprtocustom.i    ,1  F>  C:\Progress\OpenEdge\src\adm2\visprop.i  p1  �I  %C:\Progress\OpenEdge\src\adm2\custom\vispropcustom.i �1  ��  %C:\Progress\OpenEdge\src\adm2\custom\visprtocustom.i �1  �l 
 C:\Progress\OpenEdge\src\adm2\appsprop.i $2  ɏ  %C:\Progress\OpenEdge\src\adm2\custom\appspropcustom.i    X2  V  %C:\Progress\OpenEdge\src\adm2\custom\appsprtocustom.i    �2  i$  C:\Progress\OpenEdge\src\adm2\smrtprop.i �2  �j  C:\Progress\OpenEdge\gui\get 3  �  %C:\Progress\OpenEdge\src\adm2\custom\smrtpropcustom.i    <3  ��  %C:\Progress\OpenEdge\src\adm2\custom\smrtprtocustom.i    �3  ��  C:\Progress\OpenEdge\src\adm2\smrtprto.i �3  Su  C:\Progress\OpenEdge\src\adm2\globals.i  �3  M�  %C:\Progress\OpenEdge\src\adm2\custom\globalscustom.i ,4  )a  %C:\Progress\OpenEdge\src\adm2\custom\smartdefscustom.i   l4  �  C:\Progress\OpenEdge\src\adm2\appsprto.i �4  ��  %C:\Progress\OpenEdge\src\adm2\custom\appserverdefscustom.i   �4  �X 	 C:\Progress\OpenEdge\src\adm2\visprto.i  ,5  !�  %C:\Progress\OpenEdge\src\adm2\custom\visualdefscustom.i  `5  n�  C:\Progress\OpenEdge\src\adm2\cntnprto.i �5  ;  %C:\Progress\OpenEdge\src\adm2\custom\containrdefscustom.i    �5  ~�  C:\Progress\OpenEdge\src\adm2\widgetprto.i    6  e�  !C:\Progress\OpenEdge\gui\adecomm\appserv.i   X6  ��    D:\newsie\on_in_co\aplic\logis\w-estados-de-pedidos.w        J  o      �6     4  $   �6  �   �      �6  �   �     7     �     7  �   �     $7     ^     47  �   V     D7     �  #   T7  �   �     d7     �      t7  �   �     �7     �      �7  �   �     �7     �      �7  r   �     �7  n   �     �7     M  "   �7  i   H     �7     &     8  P        8  �        $8     �  !   48  �   �     D8     �     T8  �   �     d8     b     t8  �   `     �8     >     �8  g   $     �8          �8  O   �     �8  �   w     �8     u      �8  �   E     �8     �     9  �   �     9     �     $9  �   �     49     �     D9  �   �     T9     z     d9  �   y     t9     W     �9  �   F     �9     $     �9  �   !     �9     �     �9  }   �     �9     �     �9     U     �9          :     �     :  7   }     $:  �   t     4:  O   f     D:     U     T:          d:  �   �
     t:  �   �
     �:  O   �
     �:     �
     �:     I
     �:  �   $
     �:  x   
  
   �:  M   
     �:     �	     �:     �	     ;  a   �	  
   ;  �  r	     $;     S	     4;  �   	     D;  O   	     T;     	     d;     �     t;  �   �     �;     �     �;          �;  x   �     �;     �     �;     n     �;     j     �;     V     �;     =     <  Q   -  
   <     �     $<     �  
   4<     �     D<     m  
   T<  f   B     d<     �  	   t<  "   �     �<     �     �<     h     �<  Z        �<          �<     �     �<     �     �<     �     �<     |     =  4   �       =     M      $=     !       4=           