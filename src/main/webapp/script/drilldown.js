jQuery.noConflict();

var roomAtTopOfAnncontainer = 50;

var bCurrentlyDrillingDown = false;
var curAnnID = "";
var curSAWrapID = "";

jQuery(document).ready(function($){

    $(window).bind('resize', function() {
        //alert("hi");
    });

    window.drillDown = function(annid, sawrapid) {
        setCurDrillDown(true);
        curAnnID = annid;
        curSAWrapID = sawrapid;
        $('#anncontainer').scrollTo($('#' + annid));
        $('#' + curAnnID).css({
            'background':'#F0F0F0',
            'top': 0,
            'bottom': 0,
            'margin-left': 0,
            'margin-right': 0,
            'border-width': 0
        });
        $('#' + curSAWrapID).css({
            'overflow': 'auto',
            'background':'#CCCCCC',
            'border-style': 'solid',
            'border-top': '5px solid #242424',
            'height': $('#anncontainer').height() - roomAtTopOfAnncontainer
        });
        $('#anncontainer').css({
            'overflow-y': 'hidden'
        });
    }

    window.unDrillDown = function() {
        setCurDrillDown(false);
        $('#anncontainer').css('overflow-y', 'auto');
        $('#' + curSAWrapID).css({
            'height': 'auto',
            'overflow-y': 'hidden',
            'border': 'none',
            'background':'#F0F0F0',
        });
        $('#' + curAnnID).css({
            'background':'#F0F0F0',
            'top': 'auto',
            'bottom': 'auto',
            'margin-left': 5,
            'margin-right': 5,
            'border-width': '2px'
        });
    }

});


function setCurDrillDown(dd) {
    bCurrentlyDrillingDown = dd;
}

function getCurDrillDown() {
    return bCurrentlyDrillingDown;
}
