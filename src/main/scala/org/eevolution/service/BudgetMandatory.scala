/**
  * Copyright (C) 2003-2017, e-Evolution Consultants S.A. , http://www.e-evolution.com
  * This program is free software: you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation, either version 2 of the License, or
  * (at your option) any later version.
  * This program is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  * You should have received a copy of the GNU General Public License
  * along with this program.  If not, see <http://www.gnu.org/licenses/>.
  * Email: victor.perez@e-evolution.com, http://www.e-evolution.com , http://github.com/e-Evolution
  * Created by victor.perez@e-evolution.com , www.e-evolution.com
  */

package org.eevolution.service

import org.adempiere.exceptions.AdempiereException
import org.compiere.model._

/**
  * Budget Mandatory allows validate the entity and filling of dimensions
  */
trait BudgetMandatory {

  def isMandatoryProduct = false

  def isMAndatoryPartner = false

  def isMandatoryOrg = true

  def isMandatoryOrgTrx = true

  def isMandatoryActivity = true

  def isMandatoryCampaign = true

  def isMandatoryProject = true

  def isMandatoryUserElement1 = true

  def isMandatoryUserElement2 = true

  def isMandatoryUser1 = true

  def isMandatorySalesRegion = true

  def isMandatoryUser2 = true

  def isMandatoryUser3 = true

  def isMandatoryUser4 = true

  /**
    * Check Mandatory Dimension for an Entity
    *
    * @param po
    * @return
    */
  def checkMandatoryDimension(po: PO): String = {
    if (ValidCombinationService.getBudgetValidCombination(po) > 0)
      if (isMandatoryOrg && po.get_ValueAsInt(I_C_ValidCombination.COLUMNNAME_AD_Org_ID) <= 0)
        throw new AdempiereException("@AD_Org_ID@ @FillMandatory@")
    if (isMandatoryOrgTrx && po.get_ValueAsInt(I_C_ValidCombination.COLUMNNAME_AD_OrgTrx_ID) <= 0)
      throw new AdempiereException("@AD_OrgTrx_ID@ @FillMandatory@")
    if (isMandatoryProduct && po.get_ValueAsInt(I_C_ValidCombination.COLUMNNAME_M_Product_ID) <= 0)
      throw new AdempiereException("@M_Product_ID@ @FillMandatory@")
    if (isMAndatoryPartner && po.get_ValueAsInt(I_C_ValidCombination.COLUMNNAME_C_BPartner_ID) <= 0)
      throw new AdempiereException("@C_BPartner_ID@ @FillMandatory@")
    if (isMandatoryActivity && po.get_ValueAsInt(I_C_ValidCombination.COLUMNNAME_C_Activity_ID) <= 0)
      throw new AdempiereException("@C_Activity_ID@ @FillMandatory@")
    if (isMandatoryCampaign && po.get_ValueAsInt(I_C_ValidCombination.COLUMNNAME_C_Campaign_ID) <= 0)
      throw new AdempiereException("@C_Campaign_ID@ @FillMandatory@")
    if (isMandatoryProject && po.get_ValueAsInt(I_C_ValidCombination.COLUMNNAME_C_Project_ID) <= 0)
      throw new AdempiereException("@C_Project_ID@ @FillMandatory@")
    if (isMandatorySalesRegion && po.get_ValueAsInt(I_C_ValidCombination.COLUMNNAME_C_SalesRegion_ID) <= 0)
      throw new AdempiereException("@C_SalesRegion_ID@ @FillMandatory@")
    if (isMandatoryUserElement1 && po.get_ValueAsInt(I_C_ValidCombination.COLUMNNAME_UserElement1_ID) <= 0)
      throw new AdempiereException("@UserElement1_ID@ @FillMandatory@")
    if (isMandatoryUserElement2 && po.get_ValueAsInt(I_C_ValidCombination.COLUMNNAME_UserElement2_ID) <= 0)
      throw new AdempiereException("@UserElement2_ID@ @FillMandatory@")
    if (isMandatoryUser1 && po.get_ValueAsInt(I_C_ValidCombination.COLUMNNAME_User1_ID) <= 0)
      throw new AdempiereException("@User1_ID@ @FillMandatory@")
    if (isMandatoryUser2 && po.get_ValueAsInt(I_C_ValidCombination.COLUMNNAME_User2_ID) <= 0)
      throw new AdempiereException("@User2_ID@ @FillMandatory@")
    if (isMandatoryUser3 && po.get_ValueAsInt(I_C_ValidCombination.COLUMNNAME_User3_ID) <= 0)
      throw new AdempiereException("@User3_ID@ @FillMandatory@")
    if (isMandatoryUser4 && po.get_ValueAsInt(I_C_ValidCombination.COLUMNNAME_User4_ID) <= 0)
      throw new AdempiereException("@User4_ID@ @FillMandatory@")
    withoutErrors
  }
}

