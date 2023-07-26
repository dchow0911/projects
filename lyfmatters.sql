## Creating Necessary Views
Use lyfmatters;

## Views for the Chief Operating Officer
## 1. Identify the hospitals who are available 24x7 for service
CREATE OR REPLACE VIEW all_time_open_hospitals AS
SELECT 
    hospital_name, contactNumber, emailID
FROM
    hospitals
WHERE start_time = end_time;

## 2. Identify the hospitals who are at full capacity
CREATE OR REPLACE VIEW full_capacity_hospitals AS
SELECT 
    hospital_ID, hospital_name, speciality, contactNumber, emailID, bedcapacity
FROM
    hospitals
WHERE bedcapacity = beds_available;

## 3. Identify all the self owned ambulance who had their Maintenance date earlier than 6 months from today. Also identify the driver associated to it.
CREATE OR REPLACE VIEW last_maintenance_date_six_months AS
SELECT 
    e.full_name,e.phoneNumber,a.ambulance_number, a.operation_start_time, a.operation_end_time, a.last_maintenance_date
FROM
    ambulance a
        JOIN
    employee e ON a.driver = e.employeeID
WHERE
    a.last_maintenance_date < '2022-10-23' AND a.self_owned = 'Y'
ORDER BY a.last_maintenance_date;

## 4. Identify all the clinics who have no ambulance service and show the doctor in contact.
CREATE OR REPLACE VIEW clinic_with_no_ambulance AS
SELECT 
    c.clinic_name,c.address,c.city, c.state, c.zip_code, u.full_name as doctor_name, u.phoneNumber, u.personal_emailID
FROM
    clinics c
        JOIN
    users u ON c.doctor = u.user_ID
WHERE
    c.ambulance = ''
ORDER BY c.clinic_name;

## 5. Identify the doctors with recurring appointments
CREATE OR REPLACE VIEW recurring_appointments AS
SELECT 
    a.Appointment_Date as last_appointment_date, a.start_time, a.end_time, u.full_name as doctor_name, u.specialisation, u.experience, u.department
FROM
    appointments a
        JOIN
    users u ON a.doctor = u.user_ID
WHERE
    a.recurring = 1
ORDER BY a.Appointment_Date;

###################################################################################################
## Views for the Chief Executive Officer
## 1. Identify all the users who have given negative feedback
CREATE OR REPLACE VIEW negative_feedback_users AS
SELECT 
    u.full_name,u.gender,u.login_as,(curdate() - u.dateOfbirth) as age,u.personal_emailID,f.feedback_subject,f.feedback_description,f.rating
FROM
    users u
        JOIN
    feedback f ON f.given_by = u.user_ID
WHERE
    f.feedback_type = 'negative'
ORDER BY f.feedback_date;

## 2. Identify all the escalated tickets
CREATE OR REPLACE VIEW escalated_tickets AS
SELECT 
    u.full_name ,u.personal_emailID,t.issue_related_to,t.ticket_description
FROM
    users u
        JOIN
    ticket t ON u.user_ID = t.user_id
WHERE
    t.ticket_status = 'ESCALATED';

## 3. Identify count of users based on subscription
CREATE OR REPLACE VIEW users_based_on_subscription AS
SELECT 
    subscription_type, count(user_ID) as number_of_users
FROM
    users
GROUP BY subscription_type;

## 4. Identify all the users state wise
CREATE OR REPLACE VIEW statewise_users AS
SELECT 
    state ,count(user_ID)
FROM
    users
GROUP BY state;

## 5. Identify all the inactive clinics
CREATE OR REPLACE VIEW inactive_clinics AS
SELECT 
    clinic_name,email,city,state,zip_code
FROM
    clinics
WHERE active_status='0';

###################################################################################################
## Views for the Chief Financial Officer
## 1. Identify all the records whose accounts are payable
CREATE OR REPLACE VIEW accounts_payable AS
SELECT 
    transactionID,payment_method,transaction_type,transaction_date, (actual_amount-transaction_amount) as payable_amount
FROM
    expenses
WHERE transaction_amount < actual_amount;

## 2. Identify all the records whose accounts are receivable
CREATE OR REPLACE VIEW accounts_receivable AS
SELECT 
    transactionID,payment_method,transaction_type,transaction_date, (actual_amount-transaction_amount) as receivable_amount
FROM
    income
WHERE transaction_amount < actual_amount;

## 3. A customer mentioned that due to a system error, they had paid more than the actual amount, identify the record and the amount to be paid
CREATE OR REPLACE VIEW excess_amount_received AS
SELECT 
    transactionID,payment_method,transaction_type,transaction_date, ABS(actual_amount-transaction_amount) as amount_to_be_paid_back
FROM
    income
WHERE transaction_amount > actual_amount;

## 4. Find out information about the income generated from hospitals and the receivable amount, if any.
CREATE OR REPLACE VIEW hospital_income AS
SELECT 
     i.transactionID,i.payment_method,i.transaction_type,i.transaction_date, h.hospital_name, h.GSTIN, h.emailID, (i.actual_amount-i.transaction_amount) as receivable_amount
FROM
    income i
        JOIN
    hospitals h ON i.hospital_id = h.hospital_ID
ORDER BY i.transaction_date;

## 5. Provide an Expenditure report
CREATE OR REPLACE VIEW expenditure_report AS
SELECT 
    transaction_type,sum(transaction_amount) as total_expenditure, sum(actual_amount-transaction_amount) as payable_amount
FROM
    expenses
GROUP BY transaction_type;

###################################################################################################
## Views for the Chief Marketing Officer
## 1. Prepare a report for a campaign to send tips_n_info to users with certain health conditions
CREATE OR REPLACE VIEW tips_campaign AS
SELECT 
    u.full_name,u.gender,u.blood_group,u.Usual_Requirements,u.personal_emailID,h.health_condition_name,h.health_condition_description,t.title,t.tip_description
FROM
    users u
        JOIN
    health_condition h ON u.health_condition = h.health_condition_id
        JOIN
    tips_n_info t ON h.health_condition_id = t.health_condition;
    
## 2. Prepare a report to help your insurance partner in marketing various insurance offers to your patients without any insurance but facing a health issue.
CREATE OR REPLACE VIEW insurance_required AS
SELECT 
     u.full_name,u.gender,u.blood_group,u.Usual_Requirements,u.personal_emailID,h.health_condition_name,h.health_condition_description
FROM
    users u
        JOIN
    health_condition h ON u.health_condition = h.health_condition_id
WHERE u.Insurance = '0';

## 3. Prepare a referral leaderboard to send coupons to the referrers
CREATE OR REPLACE VIEW referral_leaderboard AS
SELECT 
     u.full_name,u.personal_emailID,r.referral_date
FROM
    users u
        JOIN
    referrals r ON u.Referral_code = r.Referred_code
GROUP BY u.full_name;

## 4. Identify all the Patients with Monthly plan to send them offers for yearly plan
CREATE OR REPLACE VIEW monthly_plan_users AS
SELECT
     full_name,gender,personal_emailID
FROM
    users
WHERE plan_type = 'Monthly' and subscription_type='Patient Plan';

## 5. Identify all the users with blood group as O- for a blood donation campaign
CREATE OR REPLACE VIEW blood_donation_campaign AS
SELECT
     full_name,gender,login_as,dateOfbirth,personal_emailID
FROM
    users
WHERE blood_group = 'O-';

###################################################################################################
## Views for the Chief Technical Officer
## 1. Identify all the error logs by various users
CREATE OR REPLACE VIEW error_logs AS
SELECT 
    a.created_date, a.title,a.activity_description,a.activity_type,u.login_as as user_type, u.full_name as name_of_user
FROM
    activity_tracker a
        JOIN
    users u ON a.assigned_to = u.user_ID
WHERE
    a.activity_status='Error'
ORDER BY a.created_date;

## 2. Identify all the modified logs by various users
CREATE OR REPLACE VIEW modified_logs AS
SELECT 
    a.created_date, a.last_modified_date,a.title,a.activity_description,a.activity_type,u.login_as as user_type, u.full_name as name_of_user
FROM
    activity_tracker a
        JOIN
    users u ON a.assigned_to = u.user_ID
WHERE
    NOT a.created_date=a.last_modified_date
ORDER BY a.created_date;

## 3. Identify all the users with edit access
CREATE OR REPLACE VIEW users_with_edit_access AS
SELECT 
    u.full_name, u.login_as,r.username,r.last_login
FROM
    users u
        JOIN
    user_role r ON r.user_id = u.user_ID
WHERE r.edit_access='1';

## 4. Identify all the inactive users
CREATE OR REPLACE VIEW inactive_users AS
SELECT 
    u.full_name, u.login_as,r.username,r.last_login
FROM
    users u
        JOIN
    user_role r ON r.user_id = u.user_ID
WHERE u.active_status='0';

## 5. Identify all the users who have logged in recently
CREATE OR REPLACE VIEW recent_login AS
SELECT 
    u.full_name, u.login_as,r.username,r.last_login
FROM
    users u
        JOIN
    user_role r ON r.user_id = u.user_ID
WHERE YEAR(r.last_login)='2023';

###################################################################################################
## Views for the Chief Human Resource Officer
## 1. Identify the current employees who have opted for both PF and ESI
CREATE OR REPLACE VIEW pf_esi_employee AS
SELECT 
    full_name, work_emailID, joiningDate,department,designation,current_salary
FROM
    employee
WHERE PF = '1' AND ESI = '1' AND active_status = '1';

## 2. Identify all the current employees with No. of leaves	more than 10 days this month.
CREATE OR REPLACE VIEW many_leaves AS
SELECT 
    e.full_name, e.work_emailID,e.department,e.designation,a.no_of_leaves_taken
FROM
    attendance a
        JOIN
    employee e ON a.employee_id = e.employeeID
WHERE
    a.no_of_leaves_taken>10 AND e.active_status = '1'
ORDER BY a.no_of_leaves_taken;

## 3. Identify all the current employees who have worked overtime more than 5 times this month.
CREATE OR REPLACE VIEW overtime_work AS
SELECT DISTINCT
    e.full_name, e.work_emailID,e.department,e.designation, a.no_of_overtime
FROM
    attendance a
        JOIN
    employee e ON a.employee_id = e.employeeID
WHERE
    a.no_of_overtime > 5 AND e.active_status = '1'
ORDER BY e.full_name;

## 4. Identify all the current female employees whose age is greater than 24.
CREATE OR REPLACE VIEW female_employees_more_than_24 AS
SELECT 
    full_name, work_emailID, joiningDate,department,designation,current_salary
FROM
    employee
WHERE curdate() - dateOfbirth >24 AND gender= 'F' AND active_status = '1';

## 5. Find the average salary of all employees who have work experience 5 years or more.
CREATE OR REPLACE VIEW average_salary_five_years_work_or_more AS
SELECT 
    department, AVG(current_salary) AS average_salary
FROM
    employee e
WHERE work_exp_yrs > 5
GROUP BY department
ORDER BY average_salary DESC;

##-------------------------------------------------------------------------------------------------------------------------------------
/*
## Preparing the Database Structure
CREATE DATABASE IF NOT EXISTS lyfmatters;
USE lyfmatters;

## Creating Tables
 CREATE TABLE users
 (
	active_status BOOLEAN,
    profile_photo VARCHAR(2083),
    user_ID VARCHAR(255) NOT NULL,
    role_ID VARCHAR(255) NOT NULL,
    login_as VARCHAR(255) NOT NULL,
    full_name TEXT NOT NULL,
    gender VARCHAR(255) NOT NULL,
    dateOfbirth DATE,
    blood_group VARCHAR(255) NOT NULL,
    Usual_Requirements VARCHAR(255) NOT NULL,
    phoneNumber VARCHAR(10) NOT NULL,
	personal_emailID VARCHAR(255),
	address VARCHAR(255) NOT NULL,
    city VARCHAR(255),
    state VARCHAR(255),
    zipcode VARCHAR(6) NOT NULL,
  	Medical_history BOOLEAN,
    health_condition VARCHAR(255),
	Insurance BOOLEAN,
    type_of_insurance VARCHAR(255),
    specialisation VARCHAR(255) NOT NULL,
    experience VARCHAR(255) NOT NULL,
    department VARCHAR(255) NOT NULL,
    Referral_code VARCHAR(255),
    subscription_type VARCHAR(255) NOT NULL,
    plan_type VARCHAR(255),
    hospital VARCHAR(255),
PRIMARY KEY (user_ID)
);

CREATE TABLE tips_n_info
(
	tip_id VARCHAR(255) NOT NULL PRIMARY KEY,
    title VARCHAR(255),
    tip_description VARCHAR(2083),
    category VARCHAR(255),
    health_condition VARCHAR(255)
);

CREATE TABLE health_condition
(
	health_condition_id VARCHAR (255) NOT NULL,
    health_condition_name VARCHAR(255) NOT NULL,
    health_condition_description VARCHAR(255),
	PRIMARY KEY (health_condition_id)
);

CREATE TABLE hospitals
(
	active_status BOOLEAN,
    hospital_ID VARCHAR(255) NOT NULL,
    hospital_name VARCHAR(255) NOT NULL,
	GSTIN VARCHAR(255),
    speciality VARCHAR(255) NOT NULL,
    start_time TIME,
    end_time TIME,
    bedcapacity INT(15) NOT NULL,
    beds_available INT(15) NOT NULL,
    contactNumber VARCHAR(10) NOT NULL,
	emailID VARCHAR(255),
	address VARCHAR(255) NOT NULL,
    city VARCHAR(255),
    state VARCHAR(255),
    zipcode VARCHAR(6) NOT NULL,
    location VARCHAR(2083) NOT NULL,
    website VARCHAR(2083),
    Referral_code VARCHAR(255),
    ambulance VARCHAR(255),
PRIMARY KEY (hospital_ID)
);

 CREATE TABLE clinics
 (
	active_status BOOLEAN,
    clinic_id VARCHAR(255) NOT NULL PRIMARY KEY,
    clinic_name VARCHAR(255) NOT NULL,
    GSTIN VARCHAR(255),
    address VARCHAR(255),
    city VARCHAR(255),
    state VARCHAR(255),
    zip_code INT(6) NOT NULL,
    phone_number INT(10) NOT NULL,
    email VARCHAR(255) NOT NULL,
    location VARCHAR(2083) NOT NULL,
    start_time TIME,
    end_time TIME,
    doctor VARCHAR(255),
    ambulance VARCHAR(255),
    Referral_code VARCHAR(255) NOT NULL
);

CREATE TABLE diagnostic_cnt
(
	active_status BOOLEAN,
    cnt_id VARCHAR(255) NOT NULL,
    cnt_name VARCHAR(255) NOT NULL,
    phoneNumber varchar(10) NOT NULL,
    emailID VARCHAR(255) NOT NULL,
    GSTIN VARCHAR(255),
	address VARCHAR(2083) NOT NULL,
    city VARCHAR(255),
    state VARCHAR(255),
    zipcode varchar(6) NOT NULL,
    location VARCHAR(2083) NOT NULL,
    start_time TIME,
    end_time TIME,
    ambulance VARCHAR(255),
    POC VARCHAR(255),
    Referral_code VARCHAR(255) NOT NULL,
PRIMARY KEY (cnt_id)
);

CREATE TABLE vets
(
	active_status BOOLEAN,
    vet_id VARCHAR(255) NOT NULL,
    vet_name VARCHAR(255) NOT NULL,
    phoneNumber VARCHAR(10) NOT NULL,
    emailID VARCHAR(255) NOT NULL,
    GSTIN VARCHAR(255),
	address VARCHAR(255),
    city VARCHAR(255),
    state VARCHAR(255),
    zipcode VARCHAR(6) NOT NULL,
    location VARCHAR(2083),
    doctor VARCHAR(255),
    ambulance VARCHAR(255),
    Referral_code VARCHAR(255) NOT NULL,
PRIMARY KEY (vet_id)
);

 CREATE TABLE ambulance
 (
	active_status BOOLEAN,
    ambulance_id VARCHAR(255) NOT NULL PRIMARY KEY,
    ambulance_number VARCHAR(255) NOT NULL,
    purchase_date DATE,
    operation_start_time TIME,
    operation_end_time TIME,
    self_owned BOOLEAN,
    last_maintenance_date DATE,
    driver VARCHAR(255),
    hospital VARCHAR(255),
    vet VARCHAR(255),
    diagnostic_cnt VARCHAR(255),
    clinic VARCHAR(255)
);

CREATE TABLE appointments
(
	AppointmentID VARCHAR(255) NOT NULL PRIMARY KEY,
    Appointment_Date DATE,
    doctor VARCHAR(255),
    patient VARCHAR(255),
    start_time TIME,
    end_time TIME,
    recurring BOOLEAN,
    reason VARCHAR(255)
);

CREATE TABLE referrals
(
	ReferralID VARCHAR (255) NOT NULL,
    Referred_code VARCHAR(255) NOT NULL,
    Referree VARCHAR(255) NOT NULL,
    referral_date DATE,
PRIMARY KEY (ReferralID)
);

CREATE TABLE feedback
(
	FeedbackID VARCHAR(255) NOT NULL,
    given_by VARCHAR(255) NOT NULL,
    feedback_date DATE,
    feedback_type VARCHAR(255) NOT NULL,
    feedback_subject VARCHAR(255),
    feedback_description VARCHAR(255),
    rating INT(1) NOT NULL,
    clinic_rating INT(1) NOT NULL,
PRIMARY KEY (FeedbackID)
);

 CREATE TABLE activity_tracker
 (
	activity_ID VARCHAR(255) NOT NULL PRIMARY KEY,
    assigned_to VARCHAR(255) NOT NULL,
    created_date DATE,
    title VARCHAR(255),
    activity_description VARCHAR(255),
    due_date DATE,
    activity_type VARCHAR(255),
    activity_status VARCHAR(255) DEFAULT 'Open'
);

CREATE TABLE user_role
(
	role_id VARCHAR(255) NOT NULL PRIMARY KEY,
    user_id VARCHAR(255) NOT NULL,
    username VARCHAR(255),
    last_login DATE,
    category VARCHAR(255),
    not_accessible_admin BOOLEAN,
    master_view BOOLEAN,
    master_edit BOOLEAN,
    only_view BOOLEAN,
    edit_access BOOLEAN,
    active_status BOOLEAN
);

CREATE TABLE ticket
(
	ticket_id VARCHAR (255) NOT NULL,
    ticket_owner VARCHAR(255) NOT NULL,
    user_id VARCHAR(255) NOT NULL,
    issue_related_to VARCHAR(255) NOT NULL,
    ticket_description VARCHAR(255),
    ticket_status VARCHAR(255) DEFAULT 'Pending',
PRIMARY KEY (ticket_id)
);

CREATE TABLE employee
(
	active_status BOOLEAN,
    employeeID VARCHAR(255) NOT NULL,
    full_name VARCHAR(255) NOT NULL,
    gender VARCHAR(255) NOT NULL,
    dateOfbirth DATE,
    phoneNumber VARCHAR(10) NOT NULL,
    aadharNumber VARCHAR(12)NOT NULL,
	address VARCHAR(255) NOT NULL,
    PAN_card_number VARCHAR(10),
    Bank_name VARCHAR(255),
    Bank_ac_no VARCHAR(255),
    IFSC_Code VARCHAR(255),
    personal_emailID VARCHAR(255),
    previous_org VARCHAR(255),
    work_exp_yrs INT(3),
    joiningDate DATE,
    work_emailID VARCHAR(255) NOT NULL,
    department VARCHAR(255) NOT NULL,
    designation VARCHAR(255) NOT NULL,
    current_salary INT(9) NOT NULL,
    PF BOOLEAN,
    ESI BOOLEAN,
    Profession_Tax BOOLEAN,
    TDS BOOLEAN,
    Referral_code VARCHAR(255),
PRIMARY KEY (employeeID)
);

CREATE TABLE attendance
(
	id VARCHAR(255) PRIMARY KEY,
    employee_id VARCHAR(255) NOT NULL,
    in_time TIME,
    out_time TIME,
    no_of_leaves_taken INT,
    no_of_overtime INT,
    no_of_breaks INT
);

CREATE TABLE expenses
(
	expenses_id VARCHAR(255) PRIMARY KEY,
    user_id VARCHAR(255),
    hospital_id VARCHAR(255),
    vet_id VARCHAR(255),
    clinic_id VARCHAR(255),
    cnt_id VARCHAR(255),
    ambulance_id VARCHAR(255),
    payment_method VARCHAR(255) NOT NULL,
    transaction_type VARCHAR(255) NOT NULL,
    transactionID VARCHAR(255) NOT NULL,
    transaction_date DATE,
    transaction_amount INT(25) NOT NULL,
	actual_amount INT(25) NOT NULL
);

CREATE TABLE income
(
	income_id VARCHAR(255) PRIMARY KEY,
    user_id VARCHAR(255),
    hospital_id VARCHAR(255),
    vet_id VARCHAR(255),
    clinic_id VARCHAR(255),
    cnt_id VARCHAR(255),
    ambulance_id VARCHAR(255),
    payment_method VARCHAR(255) NOT NULL,
    transaction_type VARCHAR(255) NOT NULL,
    transactionID VARCHAR(255) NOT NULL,
    transaction_date DATE,
    transaction_amount INT(25) NOT NULL,
    referral_credits_used INT(25),
    promotional_discount_applied float(25),
	actual_amount INT(25) NOT NULL
);


## Updating Tables with Foreign Keys
ALTER TABLE users
ADD FOREIGN KEY (role_ID) REFERENCES user_role(role_id),
ADD FOREIGN KEY (health_condition) REFERENCES health_condition(health_condition_id),
ADD FOREIGN KEY (hospital) REFERENCES hospitals(hospital_ID);

ALTER TABLE tips_n_info
ADD FOREIGN KEY (health_condition) REFERENCES health_condition(health_condition_id);

ALTER TABLE hospitals
ADD FOREIGN KEY (ambulance) REFERENCES ambulance(ambulance_id);

ALTER TABLE clinics
ADD FOREIGN KEY (doctor) REFERENCES users(user_ID),
ADD FOREIGN KEY (ambulance) REFERENCES ambulance(ambulance_id);

ALTER TABLE diagnostic_cnt
ADD FOREIGN KEY (POC) REFERENCES users(user_ID),
ADD FOREIGN KEY (ambulance) REFERENCES ambulance(ambulance_id);

ALTER TABLE vets
ADD FOREIGN KEY (doctor) REFERENCES users(user_ID),
ADD FOREIGN KEY (ambulance) REFERENCES ambulance(ambulance_id);

ALTER TABLE ambulance
ADD FOREIGN KEY (driver) REFERENCES employee(employeeID),
ADD FOREIGN KEY (hospital) REFERENCES hospitals(hospital_ID),
ADD FOREIGN KEY (vet) REFERENCES vets(vet_id),
ADD FOREIGN KEY (clinic) REFERENCES clinics(clinic_id),
ADD FOREIGN KEY (diagnostic_cnt) REFERENCES diagnostic_cnt(cnt_id);

ALTER TABLE appointments
ADD FOREIGN KEY (patient) REFERENCES users(user_ID),
ADD FOREIGN KEY (doctor) REFERENCES users(user_ID);

ALTER TABLE referrals
ADD FOREIGN KEY (Referree) REFERENCES users(user_ID);

ALTER TABLE feedback
ADD FOREIGN KEY (given_by) REFERENCES users(user_ID);

ALTER TABLE activity_tracker
ADD FOREIGN KEY (assigned_to) REFERENCES users(user_ID);

ALTER TABLE user_role
ADD FOREIGN KEY (user_id) REFERENCES users(user_ID);

ALTER TABLE ticket
ADD FOREIGN KEY (ticket_owner) REFERENCES users(user_ID),
ADD FOREIGN KEY (user_id) REFERENCES users(user_ID);

ALTER TABLE attendance
ADD FOREIGN KEY (employee_id) REFERENCES employee(employeeID);

ALTER TABLE expenses
ADD FOREIGN KEY (user_id) REFERENCES users(user_ID),
ADD FOREIGN KEY (hospital_id) REFERENCES hospitals(hospital_ID),
ADD FOREIGN KEY (vet_id) REFERENCES vets(vet_id),
ADD FOREIGN KEY (clinic_id) REFERENCES clinics(clinic_id),
ADD FOREIGN KEY (cnt_id) REFERENCES diagnostic_cnt(cnt_id),
ADD FOREIGN KEY (ambulance_id) REFERENCES ambulance(ambulance_id);

ALTER TABLE income
ADD FOREIGN KEY (user_id) REFERENCES users(user_ID),
ADD FOREIGN KEY (hospital_id) REFERENCES hospitals(hospital_ID),
ADD FOREIGN KEY (vet_id) REFERENCES vets(vet_id),
ADD FOREIGN KEY (clinic_id) REFERENCES clinics(clinic_id),
ADD FOREIGN KEY (cnt_id) REFERENCES diagnostic_cnt(cnt_id),
ADD FOREIGN KEY (ambulance_id) REFERENCES ambulance(ambulance_id);

ALTER TABLE activity_tracker RENAME COLUMN due_date TO last_modified_date;

##-------------------------------------------------------------------------------------------------------------------------------------
*/
