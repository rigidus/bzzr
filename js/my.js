
function ShowHide(id)
{
  if (document.getElementById(id).style.display == 'none') {
    document.getElementById(id).style.display = 'block';
  } else {
    document.getElementById(id).style.display = 'none';
  }
  return false;
}

function getBrowserInfo() {
    var t,v = undefined;

    if (window.chrome) t = 'Chrome';
    else if (window.opera) t = 'Opera';
    else if (document.all) {
        t = 'IE';
        var nv = navigator.appVersion;
        var s = nv.indexOf('MSIE')+5;
        v = nv.substring(s,s+1);
    }
    else if (navigator.appName) t = 'Netscape';

    return {type:t,version:v};
}

function bookmark(a){
    var url = window.document.location;
    var title = window.document.title;
    var b = getBrowserInfo();

    if (b.type == 'IE' && 8 >= b.version && b.version >= 4) window.external.AddFavorite('http://gdestroytorg.ru','ГдеСтройТорг.ру &mdash; Единая система поиска поставщиков');
    else if (b.type == 'Opera') {
        a.href = url;
        a.rel = "sidebar";
        a.title = url+','+title;
        return true;
    }
    else if (b.type == "Netscape") window.sidebar.addPanel('ГдеСтройТорг.ру &mdash; Единая система поиска поставщиков','http://gdestroytorg.ru',"");
    else alert("Нажмите CTRL+D, чтобы добавить страницу в Избранное.");
    return false;
}

function createDialog(){
    $('#chat_div').dialog({
                  autoOpen: true,
          buttons: {
                  'Ok': function() {
                      $(this).dialog('destroy');
                  },
                  'Cancel': function() {
                      $(this).dialog('destroy');
                  }
              }
          });
};
$('#chat').click(function(evtObj){
      createDialog();
      evtObj.preventDefault();
  $.get($(this).attr('href'), {}, function(data){
       $('#chat_div').html(data);
               createDialog();
  },'html');
});


function expandit(id){
  obj = document.getElementById(id);
  if (obj.style.display=="none") obj.style.display="";
  else obj.style.display="none";}

  $(function(){
  	// Accordion
  	$("#accordion").accordion({header: "h3" });
  	// Tabs
  	$('#tabs').tabs();
  });



