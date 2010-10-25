jQuery.noConflict();

jQuery(document).ready(function($){
    window.prependToHtml = function(id, newHtmlStr) {
        document.getElementById(id).innerHTML =
            newHtmlStr + 
            document.getElementById(id).innerHTML;
    }
});


