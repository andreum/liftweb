 function makeItem(parent, item, indent, isRTL) {
   size = item.end - item.start
   
   cssClass =  "calendarItem"
   
   if (item.cssClass) {
     cssClass = item.cssClass;
   }
   
   if (size > 1) {
     parent.append("<div id='" + item.id + "' title='" + item.description + "' indent='" + indent + "' class='" + cssClass + "' onclick='itemClick(this, \"" + item.id + "\")'><div class='" + cssClass + "Head'>" + item.startTime + "</div><div class='" + cssClass + "Body'>" + item.subject + "</div></div>");
   } else {
     parent.append("<div id='" + item.id + "' title='" + item.description + "' indent='" + indent + "' class='" + cssClass + "' onclick='itemClick(this, \"" + item.id +"\")'><div class='" + cssClass + "Head'>" + item.startTime + " " + item.subject + "</div><div class='" + cssClass + "Body'></div></div>");
   }
   
   child = $("#"+item.id);
   if (item.description) {
     child.tooltip({track: true, delay: 0, showURL: false});
   }
   
   width = child.outerWidth();

   firstIndent = 0; 
   for (k = 0; k < 8 && ($(parent.parent().children()[k]).attr("id") != $(parent).attr("id")); k++) {
      firstIndent += $(parent.parent().children()[k]).outerWidth();
   }   


   if (isRTL) {
     child.css({'right': firstIndent+indent, 'width': width - indent});
   } else {
     child.css({'left': firstIndent+indent, 'width': width - indent});
   }
   
   child.css("position", "absolute");

   h = parent.outerHeight()*size;
   child.css("height", h);
   
   h1 = $(child.children()[0]).outerHeight();
   $(child.children()[1]).height(h - h1 - 2);
}

function buildWeekViewCalendars(items) {
  if (!items) {
    items = calendars.items;
  }
  dir = $("html").attr("dir");
  isRTL = false;
  if (dir) {
    isRTL = dir.toLowerCase() == "rtl";
  }

  for (i = 0; i < items.length; i++) {
    makeItem($("#wkhidx_" + items[i].weekDay + "_" + items[i].start), items[i], makeIndent(items, i), isRTL);
  }
}

function makeIndent(items, index) {
  indent = 0;
  
  if (index > 0) {
    for (k = index - 1; k >= 0; k = k - 1) {
      if (items[index].start >= items[k].start && items[index].start < items[k].end && (items[index].weekDay == items[k].weekDay)) {
        indent = parseInt($("#"+items[k].id).attr("indent")) + 40;
        break;
      } 
    }
  }
  return indent;
}
 
function removeItem(id, items) {
  newItems = jQuery.grep(items, function(item) {
    return item.id != id;
  })
  removeAllItems(items);
  buildWeekViewCalendars(newItems);
}

function removeAllItems(items){
  jQuery.each(items, function(){
    $("#" + this.id).remove();
  })
}


