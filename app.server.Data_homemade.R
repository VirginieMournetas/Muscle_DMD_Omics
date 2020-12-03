#mRNA datatable
output$Homemade_mRNA_table <- DT::renderDataTable({
  Render_Table(mRNAseqData.full)
  #Render_Table(SQL_Table("mRNAseqData"))
})

#miR datatable
output$Homemade_miR_table <- DT::renderDataTable({
  Render_Table(miRseqData.full)
  #Render_Table(SQL_Table("miRseqData"))
})


#ALL Protein datatable
output$Homemade_protein_table <- DT::renderDataTable({
  Render_Table(ProteinData.full)
  #Render_Table(SQL_Table("ProteinData"))
})

output$Exp_mRNA <- DT::renderDataTable({
  Render_Table(SampleDescription)
  #Render_Table(SQL_Table("SampleDescription"))
})