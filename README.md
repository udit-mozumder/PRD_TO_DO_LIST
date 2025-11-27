import java.util.ArrayList;
import java.util.Date;
import java.util.List;

public class EmployeeFilterExample {

    public static void main(String[] args) {
        List<String> employees = new ArrayList<>();
        employees.add("John");
        employees.add("Alice");
        employees.add("Bob");
        employees.add("Robert");

        // Filter employees whose name starts with "R"
        List<String> filtered = new ArrayList<>();
        for (String emp : employees) {
            if (emp.startsWith("R")) {
                filtered.add(emp);
            }
        }

        System.out.println("Filtered Employees: " + filtered);

        // Thread example (anonymous runnable)
        Thread t = new Thread(new Runnable() {
            @Override
            public void run() {
                System.out.println("Thread is running at " + new Date());
            }
        });

        t.start();
    }
}
