To-Do List Software

Document Version	1.0
Document owner	Udit kumar Mozumder
Document approver	
Wireframe	https://balsamiq.cloud/slineq3/po988q6

1.	1 Goal

The objective of this project is to develop a user-friendly and efficient To-Do List software that enables users to create, organize, and track tasks seamlessly. Key features include task prioritization, due dates, categorization, reminders, and collaboration tools.

1.2 Target Audience

Individuals & Professionals

●	Teams and organizations requiring collaborative task tracking.
●	Professionals, and project managers to tracking daily work and personal tasks.

2.	User stories

User Story	Functional Requirements	Requirement in detail	Design

As a admin user, I want to create users.
●	As a admin, I want to create users.
●	As a user, I want to categorize users based on hierarchy, admin, team member and individual.
●	As a admin, I want to	
User Management
●	User authentication (login, password reset).
●	Role-based access control (admin, team member, individual user).	
●	Every user should receive an email on their mentioned email id.
●	Email should should contain an link to reset the password by the user.	
 
 
deactivate the user.			

●	As a Individual user, I want to create tasks so that I can keep track of my to- do items.
●	As a user, I want to set due dates for tasks so that I don't miss important deadlines.
●	As a user, I want to categorize tasks so that I can organize them better.
●	As a user, I want to mark tasks as completed so that I can track my progress.	
Task Management
●	Users can create, edit, and delete tasks and mark task completed.
●	Users can set due dates and priorities.
●	Users can categorize tasks using tags.
●	User can
attached PDF, Docx, JPEG/JPG files.
●	List of task will be visible in each category after a task is created.
●	Only admin will be able to delete workspace task. For Invalid task comment section can be used by tagging admin user.	
●	Module - My To-do list”
User can create more category such as:
1.	Work
2.	Personal
3.	Grocery list etc.

On double click on each task a dialog box will open with details such as:
1.	Task name
2.	Task priority
3.	Due date
4.	Time (if any)
User can create more category such as:
1.	P1
2.	P2 etc	
 
 

As a admin user, I want to create workspace and add users on that workspace.	
Collaboration Features
Assign tasks to specific users.	
Module name - Team Workspace and workspace name as subcategory.	
 
	●	Comment and discuss tasks within the app.
●	Share task lists with other users.
●	Kanban View of the Team task track.	Kanban view of the Workspace

●	Completed
●	In progress
●	To list card

Features	
	Task Assigment user rights
●	Team leader can assign task to another team member and individual user.
●	Admin can assign task to anyone.	●	All the task can be status can be changed.
●	Assignee name initials will be marked on the task for identification.
●	On double click detail view will be available
●	All the card can be renamed.
●	Admin can create more section for Kanban view.	

Reminders Section	
Calendar Section	
Features	
 
As a individual user I want a calendar section where my personal task are available and workspace task are also available.	A calendar where all the task personal and Workspace (Team work) task will be available.	1.	Calendar can be connected with google calendar using SSO
2.	On clicking on task from calendar , user will be able to view details of the task and mark comment.
3.	User will be able to change status, category,	
 
		priority of the task for both personal and workspace team task.	

Reports Section
As a user I want a report section where all the task with assigned are available.	
●	Task Status reports and Productivity tracking over time.
●	Admin can view and filter by status, assignee name.
●	Admin can view all users task.
●	Team leader can view own and team reports.
●	Individual willview their own report.	
In this reports the following column will be available:
1.	Task name
2.	Task status (To do , In progress, complete)
3.	Task assignee
4.	Start date and time ( task creation date)
5.	Due date and time (Task due date)
6.	Completion date and time( Task marked completed)
7.	Attachment if any	
 


Data management

Database Schema - MySQL can be used for structured task management and scalability

Tables:

1.	Users Table (UserID, Name, Email, Password, Role, Reports To)
2.	Tasks Table (TaskID, Title, Description, DueDate, Status, Priority, Category, AssignedTo, CreatedBy) , attachment_url
3.	Comments Table (CommentID, TaskID, UserID, CommentText, Timestamp)


Data Storage & Security

●	Secure user authentication using OAuth or JWT.
●	Encrypt sensitive data such as passwords.
●	Backup task data regularly to prevent data loss with versioning.
●	HTTPS protocol for for secure data transmission.
 
Access Control:

●	Role-based access control (RBAC) ensures restricted access.

SQL Injection Prevention:

●	Use prepared statements and parameterized queries.

Acceptance Criteria
Each feature or functionality will be considered accepted when the following conditions are met:

1.	User Management
•	Admin can successfully create, categorize (Admin, Team Member, Individual), and deactivate users.
•	Users receive an email with a secure password reset link upon account creation.
•	Login, logout, and password reset functionalities work with proper validations and error handling.
•	Role-based access control is enforced throughout the application.

2.	Task Management
•	Users can create, edit, and delete tasks.
•	Tasks can be assigned due dates, priorities (P1, P2), and categories (Work, Personal, etc.).
•	Tasks can be marked as completed and reflected in the UI accordingly.
•	Users can upload and view attachments (PDF, DOCX, and JPEG/JPG).
•	Dialog box opens with task details on double-click.

3.	Collaboration Features
•	Admin and Team Leaders can assign tasks based on hierarchy rules.
•	Task assignments reflect in the user dashboard and Kanban board.
•	Users can comment on tasks and tag other users.
•	Kanban view shows tasks under appropriate status with assignee initials.

4.	Calendar Integration
•	A calendar view shows both personal and workspace tasks.
•	Clicking a task opens its detail view with status/category updates enabled.
•	Tasks can be synced with Google Calendar (SSO-based).

5.	Reports Section
•	Users see task reports filtered by status, assignee, and completion.
 
•	Admins and team leads have extended visibility over team and individual reports.
•	Reports include all mentioned columns (task name, assignee, start/due/completion time, etc.).


Testing strategies

To create environment such as Test, UAT and Production

Unit Testing

●	Test individual components like task creation, task updates, and user authentication.

Integration Testing

●	Test interactions between the database, UI, and API endpoints.


Load test

●	Response time, Rate limit, throughput, resource utilization (CPU, memory, etc.).


User Acceptance Testing (UAT)

●	Conduct beta testing with real users to gather feedback.


Post production validation ( PPV)

●	Conduct a ppv testing to functionality end to end post deployment in the production environment.


Future Enhancements

1.	Team task automation - based on status changes

2.	UI dark mode

3.	Option to choose templates

4.	Subscription module ( Basic pack, premium pack) on basic of feature and number of users.

5.	Audit trail - Admin can view report for user activity ,users login and logout.
 
Stakeholders & Responsibilities


Stakeholder	
Responsibility

Product Manager/Product owner	
Defines vision, roadmap, and prioritizes features.

Development Team	
Development of the functionality, UI, and integrations.

QA Team	
Testing and bug fixes.

End Users	
Provide feedback, validate usability, and report issues.
 


