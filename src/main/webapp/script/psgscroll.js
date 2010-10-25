jQuery.noConflict();
var headerHeight = 60;
var wiggleRoom = 10;
var autoscrolling = false;

jQuery(document).ready(function($){

    window.focusAnnsOnUnDrilledDownSent = function(sentNum) {
        if(getCurDrillDown()) {
            unDrillDown();
        }
            $('#anncontainer').stop();
            $('#anncontainer').scrollTo( $(getAnnSentTitleID(sentNum)), {duration: 200} );
    }

    window.scrollLHSToSent = function(sentNum) {
        autoscrolling = true;
        $('#psgwrap').scrollTo('#' + sentNumToSentID(sentNum), {duration: 50});
        setTimeout('autoscrolling = false', 500);
    }

    $('#psgwrap').scroll(function () { 
        if(getCurDrillDown() || autoscrolling) return;
        $('#anncontainer').stop();
        $('#anncontainer').scrollTo( $(getAnnSentTitleID(getTopmostSentNum())), {duration: 200} );
    });

    function getTopmostSentNum() {
        var sents = $('.sent');
        var i = 0;
        var id = sents[0];
        for(var i = 0; i < sents.length; i++) {
            if($(sents[i]).offset().top - headerHeight + wiggleRoom >= 0) {
                id = $(sents[i]).attr('id');
                break;
            }
        }
        return sentIDToSentNum(id);
    }


    //So it is a convention that the sentence number X will have id sentX. This converts sentX => X.
    function sentIDToSentNum(sentID) {
        return sentID.substring(4);
    }

    //inverse fn of sentIDToSentNum:
    function sentNumToSentID(sentNum) {
        return 'sent'+sentNum;
    }

    function getAnnSentTitleID(sentNum) {
        return "#anns" + sentNum;
    }

});
