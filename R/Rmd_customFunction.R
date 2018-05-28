#' Function to create custom rmd file for reprot generation
#'
#' @description Function to create Rmd file and HTML output
#' @author Praveen baskaran \email{spemannpraveen@gmail.com}
#' @details This function isinspried from function in R packgae regionReprot
#' @param code R code(in quotes) to produce the desired out. This option takes '\'r (blackslashfollowed by r, without quotes) as line break.
#' @param sectionHash hash to indicate subsection # is section 1 and ## subsection of previous section [default #]
#' @param sectionHeader section title
#' @param chuckopts  R code chunck options in quotes [Default= "echo=FALSE,warnings=FALSE,message=FALSE"]
#' @param chunkVar  chunck variable name in single quote. This value is used as suffix to generate plot output files.
#' @param outRmd Name of the output rmd file
#' @param outRmdDir Path to output folder to save all outputs and Rmd file [Default = current working folder]
#' @param comment_after Text to appear after the plot
#' @param comment_prior Text to appear above the plot
#' @param append A logical value to append existing file or generate new file. [Default =F, generate new file]
#' @param renderOutput A logical value to render the generated Rmd to HTML document.
#' @param browse  A logical value to open the generated HTML document in browser.
#' @param logo Path to logo file
#' @param projectCode Qbic project code
#' @return Generate a Rmd file and render it to HTML.
#' @export
#' @examples
#'  createCustomRmd(code = "plot(1:10,1:10)\rplot(1:20,1:20)",append = F,outRmd = "Testing.Rmd",outRmdDir  = "../",chunkopts ="echo=FALSE")
#'  createCustomRmd(code = "for(i in 1:12){\rprint(i)\r}",append = T,sectionHeader = "heatmap",comment_after = "heatmap",outRmd = "Testing.Rmd",outRmdDir  = "../")
#'  createCustomRmd(code="plot(1:100,1:100)",sectionHash = "##",comment_prior = "This is a test",append = T,renderOutput = F,outRmd = "Testing.Rmd",sectionHeader = "subheader",outRmdDir  = "../")
#'  createCustomRmd(code="",append = T,renderOutput = T,outRmd = "Testing.Rmd",outRmdDir  = "../",projectCode="QXXXX")


createCustomRmd<-function(code="",sectionHash="#",
                          comment_after="",comment_prior="",
                          chunkopts="echo=FALSE,warnings=FALSE,message=FALSE",sectionHeader="R markdown section",chunkVar="",
                          outRmd="test.Rmd",outRmdDir=".",append=F,renderOutput=F,browse=interactive(),logo="",projectCode="QXXXX"){
  #sectionHash_arg="#"
  sectionHeader<-paste(sectionHash,sectionHeader,sep = " ")
  #comment_prior_arg="Add text info"
  chunkStart_arg<-"```{r ,chunks_code}"
  #codeArea_arg<- "plot(1:10,1:10)"
  #comment_after_arg="Add text info"
  chunkEnd<- "```"


  chunkVar<-ifelse(chunkVar!="",paste0(chunkVar,","),",")

  chunkStart<-mgsub(pattern =c(',',"chunks_code"),replacement = c(chunkVar,chunkopts),chunkStart_arg)


  if(outRmdDir %in% "."){
    outRmdDir<-getwd()
  }
  outRmd<-paste0(outRmdDir,"/",outRmd);



  if(append==T){
    if(file.exists(outRmd)){

      print(sprintf("Appending the markdown file [[ %s ]] ",outRmd))
    }else{
      stop(sprintf("The specified output markdown file [[ %s ]] does not exists",outRmd ))
    }

  }else{
    print(sprintf("A markdown file [[ %s ]] is generated", outRmd))
    dir.create(paste(outRmdDir,"Etc",sep = "/"),showWarnings = F) # create etc folder

    headerfile=gsub(pattern = ".Rmd",replacement = "_header.Rmd",x = outRmd)
    createRmdHeader_v2(headerfile =headerfile,projectCode = projectCode,logo=logo,outRmdDir = outRmdDir)

    generate_css(outRmdDir = outRmdDir) # generate corp-styles.css file
    create_bibtex(outRmdDir = outRmdDir,projectCode = projectCode)
    system(paste0("cat ", headerfile," > ", outRmd))

  }




  write(x=sectionHeader,file =outRmd,ncolumns = 1,append = T)
  write(x="",file =outRmd,ncolumns = 1,append = T)
  write(x=comment_prior,file =outRmd,ncolumns = 1,append = T)
  write(x="",file =outRmd,ncolumns = 1,append = T)
  # if code is null then dont incclude R chunks code bit
  if(code ==""){
    # do nothing
  }else{
    ###m multiline code
    ### line break "\r"
    code<-unlist(strsplit(x = code,split = "\r"))

    write(x=chunkStart,file =outRmd,ncolumns = 1,append = T)
    for(linenumeber in 1:length(code)){
      ## mutlline code split
      write(x=code[linenumeber],file =outRmd,ncolumns = 1,append = T)
    }
    write(x=chunkEnd,file =outRmd,ncolumns = 1,append = T)
  }
  write(x="",file =outRmd,ncolumns = 1,append = T)
  write(x=comment_after,file =outRmd,ncolumns = 1,append = T)
  write(x="",file =outRmd,ncolumns = 1,append = T)

  if(renderOutput == T){
    loadKnitPackage()
    rmdGenfile<-rmarkdown::render(input = outRmd,output_format = "html_document",clean = FALSE)
    if(browse){
      browseURL(rmdGenfile)
    }
  }


}






createRmdHeader_v2<-function(headerfile,device="png",projectCode=projectCode,logo=logo,outRmdDir=outRmdDir){

  template=file(headerfile, 'w')
  head="---\ntitle: \"QXXXX - Project Report\"\nsubtitle: \"Project Title\"\nauthor: \"Praveen Baskaran, Project Manager\"\ndate: \'`r format(Sys.Date(), \"%B %d, %Y\")`\'\noutput:\n  html_document:\n   toc: true\n   toc_float: true\n   toc_depth: 3\n   theme: default\n   number_sections: true\n   css: Etc/corp-styles.css\n   highlight: pygments\nbibliography: Etc/lit.bib\n---\n\n"
  head<-gsub(pattern = "QXXXX",replacement = projectCode,x = head)
  head<-gsub(pattern = "lit",replacement = projectCode,x = head)
  writeLines(text = head,con = template,sep = "\n")

  if(!logo==""){
    logo_destination<- paste(outRmdDir,"Etc","logo.png",sep = "/")
  file.copy(from = logo,to = logo_destination)
  logo_html<-'<img src="Etc/logo.png" style="position:absolute;top:0px;right:0px;" height="200" width="200" />'
  writeLines(text = logo_html,con = template,sep = "\n")
  }
  watermark<-'<div class="watermark">QBiC</div>'

  docsetup<- sprintf(c( "\n\n","```{r docSetup, bootstrap.show.code = FALSE, dev = \'%s\', bootstrap.show.message=FALSE,echo=FALSE,message=FALSE}",
                        "## knitrBoostrap and device chunk options",
                        "require(\'knitr\')",
                        "opts_chunk$set(bootstrap.show.code = FALSE, dev = \'%s\',echo=FALSE)",
                        "\n```",
                        '\\','\\','\\','\\',
                        "\n\n") ,device,device);

  projectMEmer<- c("# Projects members {-}\n",
                   "***Contact at QBiC: Praveen Baskaran***\n",
                   "QBiC\n","Auf der Morgenstelle 10\n","72076 Tübingen\n",
                   "praveen.baskaran@qbic.uni-tuebingen.de\n","+49 7071 29 70296\n","***\n")

  writeLines(text = watermark,con = template,sep = "\n")
  for( each in c(docsetup,projectMEmer)){
    writeLines(text = each,con = template,sep = "\n");
  }


  close(template)
}



loadKnitPackage<-function(){
  pkgTest("knitr" )
  pkgTest("knitrBootstrap")
  pkgTest("knitcitations")
  pkgTest("DT")
  pkgTest("rmarkdown")
  pkgTest("BiocStyle")
  pkgTest("tufte")
  pkgTest("tableHTML")
}


mgsub <- function(pattern, replacement, x, ...) {
  if (length(pattern)!=length(replacement)) {
    stop("pattern and replacement do not have the same length.")
  }
  result <- x
  for (i in 1:length(pattern)) {
    result <- gsub(pattern[i], replacement[i], result, ...)
  }
  return(result)
}


generate_css<-function(outRmdDir){
require(tableHTML)
 mycss<-make_css(list('body','font-family','Calibri, helvetica, sans-serif'),
                 list('h1',c('color','font-size'),c('rgb(3, 101, 192)','127%')),
                 list('.title','margin-right','200px'),
                 list('h2',c('color','font-size'),c('rgb(3, 101, 192)','121%')),
                 list('h3',c('font-size','font-weight'),c('109%','bold')),
                 list('h4',c('font-size','font-weight','font-style'),c('100%','bold','italic')),
                 list('.watermark',c('opacity','position','top','left','font-size','color'),c('0.1','fixed','50%','50%','500%','#00407d')))
outcss<-paste(outRmdDir,"Etc","corp-styles.css",sep = "/")
 write(mycss,file =outcss)
}




createRmdHeader<-function(headerfile,device="png",projectCode=""){

  template=file(headerfile, 'w')
  head="---\noutput:\n  html_document2:\n    toc: true\n    toc_float: true\n    code_folding: none\n  html_document:\n    toc: true\n    toc_float: true\n    code_folding: none\n    theme: spacelab\n  pdf_document:\n    toc: true\n  knitrBootstrap::bootstrap_document:\n    theme.chooser: TRUE\n    highlight.chooser: TRUE\n---\n\n"

  docsetup<- sprintf(c( "```{r docSetup, bootstrap.show.code = FALSE, dev = \'%s\', bootstrap.show.message=FALSE,echo=FALSE,message=FALSE}",
                        "## knitrBoostrap and device chunk options",
                        "require(\'knitr\')",
                        "opts_chunk$set(bootstrap.show.code = FALSE, dev = \'%s\',echo=FALSE)",
                        "\n```",
                        "Qbic Report for project:: %s",
                        "================================\n\n") ,device,device,projectCode);

  writeLines(text = head,con = template,sep = "\n")
  for( each in docsetup){
    writeLines(text = each,con = template,sep = "\n");
  }

  close(template)
}



# create a bibtex file with citation for RcoreTeam
create_bibtex<-function(outRmdDir,projectCode){

  r<-toBibtex(citation("base"))
  r[1]<-gsub(pattern = ",",replacement = "Rcoreteam2017,",x = r[1])
  outbib<-paste(outRmdDir,"/","Etc","/",projectCode,".bib",sep="")

write(r,file = outbib)
}

#render(input = "Testing.Rmd",output_format = "html_document2")


#createCustomRmd(code = "plot(1:10,1:10)\rplot(1:20,1:20)",append = F,outRmd = "Testing.Rmd",outRmdDir  = "../",chunkopts ="echo=FALSE")
#createCustomRmd(code = "for(i in 1:12){\rprint(i)\r}",append = T,sectionHeader = "heatmap",comment_after = "heatmap",outRmd = "Testing.Rmd",outRmdDir  = "../")
#createCustomRmd(code="plot(1:100,1:100)",sectionHash = "##",comment_prior = "This is a test",append = T,renderOutput = T,outRmd = "Testing.Rmd",sectionHeader = "subheader",outRmdDir  = "../")

