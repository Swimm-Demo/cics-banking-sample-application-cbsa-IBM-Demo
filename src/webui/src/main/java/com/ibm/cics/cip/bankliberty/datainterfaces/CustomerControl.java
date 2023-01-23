/*
 *
 *    Copyright IBM Corp. 2022
 *
 */
package com.ibm.cics.cip.bankliberty.datainterfaces;

import com.ibm.jzos.fields.*;

// Generated by IBM Record Generator for Java V3.0.0 Build 20170904-1704 on: Fri Mar 13 11:18:25 GMT 2020

public class CustomerControl
{

	static final String COPYRIGHT = "Copyright IBM Corp. 2022";

	protected static CobolDatatypeFactory factory = new CobolDatatypeFactory();
	static
	{
		factory.setStringTrimDefault(false);
	}

	/**
	 * <pre>
	 01 COBOL-LANGUAGE-STRUCTURE.
	 * </pre>
	 */
	public static final int COBOL_LANGUAGE_STRUCTURE_LEN = 329;

	/**
	 * <pre>
	     03 CUSTOMER-CONTROL-RECORD.
	 * </pre>
	 */
	public static final int CUSTOMER_CONTROL_RECORD_LEN = 329;

	public static final int CUSTOMER_CONTROL_RECORD_OFFSET = factory
			.getOffset();

	/**
	 * <pre>
	        05 CUSTOMER-CONTROL-EYECATCHER             PIC X(4).
	 * </pre>
	 */
	protected static final StringField CUSTOMER_CONTROL_EYECATCHER = factory
			.getStringField(4);

	/**
	 * <pre>
	           88 CUSTOMER-CONTROL-EYECATCHER-V        VALUE 'CTRL'.
	 * </pre>
	 */
	public static final String CUSTOMER_CONTROL_EYECATCHER_V = "CTRL";

	/**
	 * <pre>
	        05 CUSTOMER-CONTROL-KEY.
	 * </pre>
	 */
	public static final int CUSTOMER_CONTROL_KEY_LEN = 16;

	public static final int CUSTOMER_CONTROL_KEY_OFFSET = factory.getOffset();

	/**
	 * <pre>
	           07 CUSTOMER-CONTROL-SORTCODE        PIC 9(6) DISPLAY.
	 * </pre>
	 */
	protected static final ExternalDecimalAsIntField CUSTOMER_CONTROL_SORTCODE = factory
			.getExternalDecimalAsIntField(6, false, false, false, false);

	/**
	 * <pre>
	           07 CUSTOMER-CONTROL-NUMBER          PIC 9(10) DISPLAY.
	 * </pre>
	 */
	protected static final ExternalDecimalAsLongField CUSTOMER_CONTROL_NUMBER = factory
			.getExternalDecimalAsLongField(10, false, false, false, false);

	/**
	 * <pre>
	        05 NUMBER-OF-CUSTOMERS                 PIC 9(10) DISPLAY.
	 * </pre>
	 */
	protected static final ExternalDecimalAsLongField NUMBER_OF_CUSTOMERS = factory
			.getExternalDecimalAsLongField(10, false, false, false, false);

	/**
	 * <pre>
	        05 LAST-CUSTOMER-NUMBER                PIC 9(10) DISPLAY.
	 * </pre>
	 */
	protected static final ExternalDecimalAsLongField LAST_CUSTOMER_NUMBER = factory
			.getExternalDecimalAsLongField(10, false, false, false, false);

	/**
	 * <pre>
	        05 CUSTOMER-CONTROL-SUCCESS-FLAG       PIC X.
	 * </pre>
	 */
	protected static final StringField CUSTOMER_CONTROL_SUCCESS_FLAG = factory
			.getStringField(1);

	/**
	 * <pre>
	        88 CUSTOMER-CONTROL-SUCCESS VALUE 'Y'.
	 * </pre>
	 */
	public static final String CUSTOMER_CONTROL_SUCCESS = "Y";

	/**
	 * <pre>
	        05 CUSTOMER-CONTROL-FAIL-CODE PIC X.
	 * </pre>
	 */
	protected static final StringField CUSTOMER_CONTROL_FAIL_CODE = factory
			.getStringField(1);

	/**
	 * <pre>
	        05 FILLER                              PIC X(38).
	 * </pre>
	 */
	protected static final StringField FILLER_1 = factory.getStringField(38);

	/**
	 * <pre>
	        05 FILLER                              PIC X(160).
	 * </pre>
	 */
	protected static final StringField FILLER_2 = factory.getStringField(160);

	/**
	 * <pre>
	        05 FILLER                              PIC 9(8).
	 * </pre>
	 */
	protected static final ExternalDecimalAsIntField FILLER_3 = factory
			.getExternalDecimalAsIntField(8, false, false, false, false);

	/**
	 * <pre>
	        05 FILLER                              PIC 999.
	 * </pre>
	 */
	protected static final ExternalDecimalAsIntField FILLER_4 = factory
			.getExternalDecimalAsIntField(3, false, false, false, false);

	/**
	 * <pre>
	        05 FILLER                              PIC 9(8).
	 * </pre>
	 */
	protected static final ExternalDecimalAsIntField FILLER_5 = factory
			.getExternalDecimalAsIntField(8, false, false, false, false);

	/**
	 * <pre>
	        05 FILLER                              PIC 9(11).
	 * </pre>
	 */
	protected static final ExternalDecimalAsLongField FILLER_6 = factory
			.getExternalDecimalAsLongField(11, false, false, false, false);

	/**
	 * <pre>
	        05 FILLER                              PIC X(50).
	 * </pre>
	 */
	protected static final StringField FILLER_7 = factory.getStringField(50);

	/**
	 * <pre>
	        05 FILLER                              PIC X(9).
	 * </pre>
	 */
	protected static final StringField FILLER_8 = factory.getStringField(9);

	protected byte[] byteBuffer;

	// Instance variables used to cache field values
	protected String customerControlEyecatcher;

	protected Integer customerControlSortcode;

	protected Long customerControlNumber;

	protected Long numberOfCustomers;

	protected Long lastCustomerNumber;

	protected String customerControlSuccessFlag;

	protected String customerControlFailCode;


	public CustomerControl(byte[] buffer)
	{
		this.byteBuffer = buffer;
	}


	public CustomerControl()
	{
		this.byteBuffer = new byte[COBOL_LANGUAGE_STRUCTURE_LEN];
	}


	public byte[] getByteBuffer()
	{
		return byteBuffer;
	}


	public String getCustomerControlEyecatcher()
	{
		if (customerControlEyecatcher == null)
		{
			customerControlEyecatcher = CUSTOMER_CONTROL_EYECATCHER
					.getString(byteBuffer);
		}
		return customerControlEyecatcher;
	}


	public void setCustomerControlEyecatcher(String customerControlEyecatcher)
	{
		if (CUSTOMER_CONTROL_EYECATCHER.equals(this.customerControlEyecatcher,
				customerControlEyecatcher))
		{
			return;
		}
		CUSTOMER_CONTROL_EYECATCHER.putString(customerControlEyecatcher,
				byteBuffer);
		this.customerControlEyecatcher = customerControlEyecatcher;
	}


	public boolean isCustomerControlEyecatcherV()
	{
		return getCustomerControlEyecatcher()
				.equals(CUSTOMER_CONTROL_EYECATCHER_V);
	}


	public int getCustomerControlSortcode()
	{
		if (customerControlSortcode == null)
		{
			customerControlSortcode = CUSTOMER_CONTROL_SORTCODE
					.getInt(byteBuffer);
		}
		return customerControlSortcode.intValue();
	}


	public void setCustomerControlSortcode(int customerControlSortcode)
	{
		if (CUSTOMER_CONTROL_SORTCODE.equals(this.customerControlSortcode,
				customerControlSortcode))
		{
			return;
		}
		CUSTOMER_CONTROL_SORTCODE.putInt(customerControlSortcode, byteBuffer);
		this.customerControlSortcode = customerControlSortcode;
	}


	public long getCustomerControlNumber()
	{
		if (customerControlNumber == null)
		{
			customerControlNumber = CUSTOMER_CONTROL_NUMBER.getLong(byteBuffer);
		}
		return customerControlNumber.longValue();
	}


	public void setCustomerControlNumber(long customerControlNumber)
	{
		if (CUSTOMER_CONTROL_NUMBER.equals(this.customerControlNumber,
				customerControlNumber))
		{
			return;
		}
		CUSTOMER_CONTROL_NUMBER.putLong(customerControlNumber, byteBuffer);
		this.customerControlNumber = customerControlNumber;
	}


	public long getNumberOfCustomers()
	{
		if (numberOfCustomers == null)
		{
			numberOfCustomers = NUMBER_OF_CUSTOMERS.getLong(byteBuffer);
		}
		return numberOfCustomers.longValue();
	}


	public void setNumberOfCustomers(long numberOfCustomers)
	{
		if (NUMBER_OF_CUSTOMERS.equals(this.numberOfCustomers,
				numberOfCustomers))
		{
			return;
		}
		NUMBER_OF_CUSTOMERS.putLong(numberOfCustomers, byteBuffer);
		this.numberOfCustomers = numberOfCustomers;
	}


	public long getLastCustomerNumber()
	{
		if (lastCustomerNumber == null)
		{
			lastCustomerNumber = LAST_CUSTOMER_NUMBER.getLong(byteBuffer);
		}
		return lastCustomerNumber.longValue();
	}


	public void setLastCustomerNumber(long lastCustomerNumber)
	{
		if (LAST_CUSTOMER_NUMBER.equals(this.lastCustomerNumber,
				lastCustomerNumber))
		{
			return;
		}
		LAST_CUSTOMER_NUMBER.putLong(lastCustomerNumber, byteBuffer);
		this.lastCustomerNumber = lastCustomerNumber;
	}


	public String getCustomerControlSuccessFlag()
	{
		if (customerControlSuccessFlag == null)
		{
			customerControlSuccessFlag = CUSTOMER_CONTROL_SUCCESS_FLAG
					.getString(byteBuffer);
		}
		return customerControlSuccessFlag;
	}


	public void setCustomerControlSuccessFlag(String customerControlSuccessFlag)
	{
		if (CUSTOMER_CONTROL_SUCCESS_FLAG.equals(
				this.customerControlSuccessFlag, customerControlSuccessFlag))
		{
			return;
		}
		CUSTOMER_CONTROL_SUCCESS_FLAG.putString(customerControlSuccessFlag,
				byteBuffer);
		this.customerControlSuccessFlag = customerControlSuccessFlag;
	}


	public boolean isCustomerControlSuccess()
	{
		return getCustomerControlSuccessFlag().equals(CUSTOMER_CONTROL_SUCCESS);
	}


	public String getCustomerControlFailCode()
	{
		if (customerControlFailCode == null)
		{
			customerControlFailCode = CUSTOMER_CONTROL_FAIL_CODE
					.getString(byteBuffer);
		}
		return customerControlFailCode;
	}


	public void setCustomerControlFailCode(String customerControlFailCode)
	{
		if (CUSTOMER_CONTROL_FAIL_CODE.equals(this.customerControlFailCode,
				customerControlFailCode))
		{
			return;
		}
		CUSTOMER_CONTROL_FAIL_CODE.putString(customerControlFailCode,
				byteBuffer);
		this.customerControlFailCode = customerControlFailCode;
	}

}
