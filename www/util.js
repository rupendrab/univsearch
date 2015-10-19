$( document ).ready(function() {
   function showTab(tabName) {
     destTab = $("a[data-value='" + tabName + "']")
     if ( ! destTab.parent().hasClass("active") ) {
       destTab.click()
     }
   }

   $("#goButton").click(function(event){
     showTab('Results')
     /*
     resultTab = $("a[data-value='Results']")
     if ( ! resultTab.parent().hasClass("active") ) {
       resultTab.click()
     }
     */
   });

   $("#helpButton").click(function(event) {
     showTab('Help')
   });
});
