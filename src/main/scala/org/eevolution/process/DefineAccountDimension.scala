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

package org.eevolution.process

import org.eevolution.service.{ValidCombinationService}

/**
  * Define Account Dimension
  * This process allows fill all dimension for any entity
  * Created by e-Evolution on 08/04/17.
  */
class DefineAccountDimension extends DefineAccountDimensionAbstract {

  /**
    * prepare process
    */
  override protected def prepare(): Unit = super.prepare()

  /**
    * Do It Define Account Dimension
    *
    * @return
    */
  override def doIt(): String = {
    if (getSelectionKeys != null && getSelectionKeys.size() > 0) {
      val selectionId = getSelectionKeys.stream().findFirst().get()
      val instance = getInstance(get_TrxName())
      if (instance != null) {
        // Fill dimension
        ValidCombinationService.fillDimension(selectionId, instance)(getCtx, get_TrxName())
        // Validate invoice
      }
    }
    "@Ok@"
  }

}
