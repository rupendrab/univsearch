$( document ).ready(function() {
   function showTab(tabName) {
     destTab = $("a[data-value='" + tabName + "']")
     if ( ! destTab.parent().hasClass("active") ) {
       destTab.click()
     }
   }

   $("#goButton").click(function(event){
     showTab('Results')
   });

   $("#helpButton").click(function(event) {
     showTab('Help')
   });
});
