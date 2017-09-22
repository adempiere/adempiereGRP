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

import java.util.Properties

import org.adempiere.exceptions.AdempiereException
import org.compiere.model._
import org.compiere.util.DB

/**
  * Valid Combination Service
  * Created by e-Evolution on 08/04/17.
  */
object ValidCombinationService extends BudgetMandatory {

  /**
    * Fill Dimension
    *
    * @param accountId
    * @param instance
    * @param context
    * @param trxName
    */
  def fillDimension(accountId: Int, instance: PO)(implicit context: Properties, trxName: String): Unit = {
    val account = new MAccount(context, accountId, trxName)
    if (instance.get_ColumnIndex(I_C_ValidCombination.COLUMNNAME_AD_Org_ID) > 0
      && instance.get_ValueAsInt(I_C_ValidCombination.COLUMNNAME_AD_Org_ID) != account.getAD_Org_ID)
      instance.set_ValueOfColumn(I_C_ValidCombination.COLUMNNAME_AD_Org_ID, account.getAD_Org_ID)
    if (instance.get_ColumnIndex(I_C_ValidCombination.COLUMNNAME_AD_OrgTrx_ID) > 0
      && instance.get_ValueAsInt(I_C_ValidCombination.COLUMNNAME_AD_OrgTrx_ID) != account.getAD_OrgTrx_ID)
      instance.set_ValueOfColumn(I_C_ValidCombination.COLUMNNAME_AD_OrgTrx_ID, account.getAD_OrgTrx_ID)
    if (instance.get_ColumnIndex(I_C_ValidCombination.COLUMNNAME_C_Activity_ID) > 0
      && instance.get_ValueAsInt(I_C_ValidCombination.COLUMNNAME_C_Activity_ID) != account.getC_Activity_ID)
      instance.set_ValueOfColumn(I_C_ValidCombination.COLUMNNAME_C_Activity_ID, account.getC_Activity_ID)
    if (instance.get_ColumnIndex(I_C_ValidCombination.COLUMNNAME_C_Campaign_ID) > 0
      && instance.get_ValueAsInt(I_C_ValidCombination.COLUMNNAME_C_Campaign_ID) != account.getC_Campaign_ID)
      instance.set_ValueOfColumn(I_C_ValidCombination.COLUMNNAME_C_Campaign_ID, account.getC_Campaign_ID)
    if (instance.get_ColumnIndex(I_C_ValidCombination.COLUMNNAME_C_Project_ID) > 0
      && instance.get_ValueAsInt(I_C_ValidCombination.COLUMNNAME_C_Project_ID) != account.getC_Project_ID)
      instance.set_ValueOfColumn(I_C_ValidCombination.COLUMNNAME_C_Project_ID, account.getC_Project_ID)
    if (instance.get_ColumnIndex(I_C_ValidCombination.COLUMNNAME_C_SalesRegion_ID) > 0
      && instance.get_ValueAsInt(I_C_ValidCombination.COLUMNNAME_C_SalesRegion_ID) != account.getC_SalesRegion_ID)
      instance.set_ValueOfColumn(I_C_ValidCombination.COLUMNNAME_C_SalesRegion_ID, account.getC_SalesRegion_ID)
    if (instance.get_ColumnIndex(I_C_ValidCombination.COLUMNNAME_User1_ID) > 0
      && instance.get_ValueAsInt(I_C_ValidCombination.COLUMNNAME_User1_ID) != account.getUser1_ID)
      instance.set_ValueOfColumn(I_C_ValidCombination.COLUMNNAME_User1_ID, account.getUser1_ID)
    if (instance.get_ColumnIndex(I_C_ValidCombination.COLUMNNAME_User2_ID) > 0
      && instance.get_ValueAsInt(I_C_ValidCombination.COLUMNNAME_User2_ID) != account.getUser2_ID)
      instance.set_ValueOfColumn(I_C_ValidCombination.COLUMNNAME_User2_ID, account.getUser2_ID)
    if (instance.get_ColumnIndex(I_C_ValidCombination.COLUMNNAME_User3_ID) > 0
      && instance.get_ValueAsInt(I_C_ValidCombination.COLUMNNAME_User3_ID) != account.getUser3_ID)
      instance.set_ValueOfColumn(I_C_ValidCombination.COLUMNNAME_User3_ID, account.getUser3_ID)
    if (instance.get_ColumnIndex(I_C_ValidCombination.COLUMNNAME_User4_ID) > 0
      && instance.get_ValueAsInt(I_C_ValidCombination.COLUMNNAME_User4_ID) != account.getUser4_ID)
      instance.set_ValueOfColumn(I_C_ValidCombination.COLUMNNAME_User4_ID, account.getUser4_ID)
    if (instance.get_ColumnIndex(I_C_ValidCombination.COLUMNNAME_UserElement1_ID) > 0
      && instance.get_ValueAsInt(I_C_ValidCombination.COLUMNNAME_UserElement1_ID) != account.getUserElement1_ID)
      instance.set_ValueOfColumn(I_C_ValidCombination.COLUMNNAME_UserElement1_ID, account.getUserElement1_ID)
    if (instance.get_ColumnIndex(I_C_ValidCombination.COLUMNNAME_UserElement2_ID) > 0
      && instance.get_ValueAsInt(I_C_ValidCombination.COLUMNNAME_UserElement2_ID) != account.getUserElement2_ID)
      instance.set_ValueOfColumn(I_C_ValidCombination.COLUMNNAME_UserElement2_ID, account.getUserElement2_ID)
    if (I_C_InvoiceLine.Table_ID == instance.get_Table_ID
      && instance.get_ColumnIndex(I_C_ValidCombination.COLUMNNAME_C_ValidCombination_ID) > 0
      && instance.get_ValueAsInt(I_C_ValidCombination.COLUMNNAME_C_ValidCombination_ID) != account.get_ID) // todo : is necessary change the C_ValidCombination_ID for BudgetValidCombination_ID
      instance.set_ValueOfColumn(I_C_ValidCombination.COLUMNNAME_C_ValidCombination_ID, account.get_ID)
    if (I_GL_JournalLine.Table_ID == instance.get_Table_ID
      && instance.get_ColumnIndex("BudgetValidCombination_ID") > 0
      && instance.get_ValueAsInt("BudgetValidCombination_ID") != account.get_ID)
      instance.set_ValueOfColumn("BudgetValidCombination_ID", account.get_ID)
    if (I_C_BankStatementLine.Table_ID == instance.get_Table_ID
      && instance.get_ColumnIndex(I_C_ValidCombination.COLUMNNAME_C_ValidCombination_ID) > 0
      && instance.get_ValueAsInt(I_C_ValidCombination.COLUMNNAME_C_ValidCombination_ID) != account.get_ID) // todo : is necessary change the C_ValidCombination_ID for BudgetValidCombination_ID
      instance.set_ValueOfColumn(I_C_ValidCombination.COLUMNNAME_C_ValidCombination_ID, account.get_ID)
    if (instance.is_Changed())
      instance.saveEx()
  }

  /**
    * Model Validator after new
    *
    * @param po
    * @return
    */
  def afterNew(po: PO): String = {
    val optioElementValue = Option(DB.getSQLValueString(po.get_TrxName, "SELECT Value FROM C_ElementValue WHERE C_ElementValue_ID = ?", po.get_ValueAsInt(I_C_ValidCombination.COLUMNNAME_Account_ID)))
    if (optioElementValue.isDefined && BudgetToBeExercised == optioElementValue.get)
      checkMandatoryDimension(po)
    checkDuplicateAlias(Option(po.asInstanceOf[MAccount]))
    withoutErrors
  }

  def checkDuplicateAlias(accountOption: Option[Account]): Unit = {
    accountOption.foreach(account => {
      //get valid combination based on account alias
      val alias = Option(account.getAlias).getOrElse(getAlias(
        account.getAD_Org_ID,
        account.getAD_OrgTrx_ID,
        account.getC_Activity_ID,
        account.getC_Campaign_ID,
        account.getC_Project_ID,
        account.getUserElement1_ID,
        account.getUserElement2_ID,
        account.getUser1_ID,
        account.getC_SalesRegion_ID,
        account.getUser2_ID,
        account.getUser3_ID,
        account.getUser4_ID)(account.get_TrxName()))
      val optioElementValue = Option(DB.getSQLValueString(account.get_TrxName, "SELECT Value FROM C_ElementValue WHERE C_ElementValue_ID = ?", account.getAccount_ID))
      if (optioElementValue.isDefined && (BudgetToBeExercised == optioElementValue.get || BudgetExecute == optioElementValue.get)) {
        val validCombinatonOption = getValidCombiantion(alias)(account.getCtx, account.get_TrxName())
        validCombinatonOption.foreach(validCombination => {
          if (validCombination.get_ID == account.get_ID)
            throw new AdempiereException(s" Clave Presupuestal ya existe : $alias")
        })
        //if valid combination is empty then update alias because not exists
        if (validCombinatonOption.isEmpty) {
          account.setAlias(alias)
          account.saveEx()
        }
      }
    })
  }

  /**
    * get Valid Combination based in budget available account
    *
    * @param alias
    * @param context
    * @param trxName
    * @return
    */
  def getValidCombiantion(alias: String)(implicit context: Properties, trxName: String): Option[MAccount] = {
    val where = new StringBuilder
    where.append(I_C_ValidCombination.COLUMNNAME_Alias).append("=? AND ")
    where.append("EXISTS (SELECT 1 FROM  C_ElementValue ev WHERE  ev.C_ElementValue_ID=C_ValidCombination.Account_ID AND Value = ?)")
    val optionAccount = Option(new Query(context, I_C_ValidCombination.Table_Name, where.toString(), trxName)
      .setClient_ID()
      .setOnlyActiveRecords(true)
      .setParameters(
        alias,
        BudgetToBeExercised)
      first())
    optionAccount
  }

  /**
    * Get Alias based on dimensions
    *
    * @param orgId
    * @param orgTrxId
    * @param activityId
    * @param campaignId
    * @param projectId
    * @param userElement1Id
    * @param userElement2Id
    * @param user1Id
    * @param salesRegionId
    * @param user2Id
    * @param user3Id
    * @param user4Id
    * @return
    */
  def getAlias(orgId: Int,
               orgTrxId: Int,
               activityId: Int,
               campaignId: Int,
               projectId: Int,
               userElement1Id: Int,
               userElement2Id: Int,
               user1Id: Int,
               salesRegionId: Int,
               user2Id: Int,
               user3Id: Int,
               user4Id: Int)(implicit trxName: String): String = {
    val dependence = Option(DB.getSQLValueString(trxName, "SELECT value FROM AD_Org WHERE AD_Org_ID=?", orgId))
    val unitAdmin = Option(DB.getSQLValueString(trxName, "SELECT value FROM AD_Org WHERE AD_Org_ID=?", orgTrxId))
    val clasificationAdmin = Option(DB.getSQLValueString(trxName, "SELECT value FROM C_Activity WHERE C_Activity_ID=?", activityId))
    val purpose = Option(DB.getSQLValueString(trxName, "SELECT value FROM C_Campaign WHERE C_Campaign_ID=?", campaignId))
    val program = Option(DB.getSQLValueString(trxName, "SELECT value FROM C_Project WHERE C_Project_ID=?", projectId))
    val goal = Option(DB.getSQLValueString(trxName, "SELECT name FROM PA_Goal WHERE PA_Goal_ID=?", userElement1Id))
    val vulnerableGroup = Option(DB.getSQLValueString(trxName, "SELECT value FROM R_InterestArea WHERE R_InterestArea_ID=?", userElement2Id))
    val budgetItem = Option(DB.getSQLValueString(trxName, "SELECT value FROM C_ElementValue WHERE C_ElementValue_ID=?", user1Id))
    val budgetClass = Option(DB.getSQLValueString(trxName, "SELECT value FROM C_SalesRegion WHERE C_SalesRegion_ID=?", salesRegionId))
    val expenseType = Option(DB.getSQLValueString(trxName, "SELECT value FROM C_ElementValue WHERE C_ElementValue_ID=?", user2Id))
    val economicClassification = Option(DB.getSQLValueString(trxName, "SELECT value FROM C_ElementValue WHERE C_ElementValue_ID=?", user3Id))
    val financialSource = Option(DB.getSQLValueString(trxName, "SELECT value FROM C_ElementValue WHERE C_ElementValue_ID=?", user4Id))
    val alias = unitAdmin.getOrElse("") +
      clasificationAdmin.getOrElse("") +
      purpose.getOrElse("") +
      program.getOrElse("") +
      goal.getOrElse("") +
      vulnerableGroup.getOrElse("") +
      budgetItem.getOrElse("") +
      budgetClass.getOrElse("") +
      expenseType.getOrElse("") +
      economicClassification.getOrElse("") +
      financialSource.getOrElse("")
    alias
  }
}

