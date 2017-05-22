#' @title LTER lakes Northern Ice data
#' 
#' @description 
#' Return the information on ice on/out for the LTER lakes. 
#' 
#' @note 
#' Northern and southern ice on/off data are not described in exactly the same way, so some liberty was taken here. 
#' Southern data are reported only as "ice_on" and "ice_off". To match the northern data, I have renamed them 
#' "datelastopen" and "datefirstopen" respectively. I feel this is the closest analog. 
#' 
#' Further, there is a "year" column. There is no clear consensus on what season or year should correspond with 
#' in ice dates. For example, sometimes ice-on can be in December, and sometimes in January. What "year" is that 
#' ice date associated with? Here, I have normalized everything so that "year" corresponds with the *summer* after
#' an ice-out date and before the ice-on date. So ice-on in January 1984 corresponds to the "year" 1983. 
#' 
#' 
#' @import httr
#' @import dplyr
#' @importFrom lubridate mdy
#' @import utils
#' 
#' @export
lter_ice_onoff = function(){
  
  r = GET('https://lter.limnology.wisc.edu/file/11470/download?token=HNlsCKw9OOZTgFLLcnpwCHPxhlrBHBeVtcXlvfQVAXs')
  north = content(r) %>% select(lakeid, year, datefirstopen, datelastopen, datelastice, datefirstice)
  
  r = GET('https://lter.limnology.wisc.edu/file/11471/download?token=nXC_mWDNhqvwGL8JiqS2Zu9Dg1qGK6Jq0FzmBYFT1E8')
  south = content(r) %>% transmute(lakeid, year=year4, datelastopen=mdy(ice_on), datefirstopen=mdy(ice_off), 
                                   datelastice=NA, datefirstice=NA)
  
  #ok, now we must lag
  south = group_by(south, lakeid) %>% mutate(datefirstopen=lag(datefirstopen)) %>% tail
  
  #d$date = as.POSIXct(d$sampledate)
  
  d = rbind(north, south)
  
  return(as.data.frame(d))
}