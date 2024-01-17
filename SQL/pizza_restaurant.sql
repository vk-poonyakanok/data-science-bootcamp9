-- customers table
CREATE TABLE customers (
    customer_id INT PRIMARY KEY,
    name VARCHAR(100),
    phone_number VARCHAR(20),
    email VARCHAR(100)
);

-- orders table
CREATE TABLE orders (
    order_id INT PRIMARY KEY,
    customer_id INT,
    order_date DATE,
    total_price DECIMAL(5,2),
    FOREIGN KEY (customer_id) REFERENCES customers(customer_id)
);

-- pizza menu table
CREATE TABLE menu (
  pizza_id INT PRIMARY KEY,
  toppings VARCHAR(100),
  price DECIMAL(4,2)
);

-- Inserting example data into the customers table
INSERT INTO customers (customer_id, name, phone_number, email) VALUES
  (1, 'Akira Tanaka', '080-1111-2222', 'akira.tanaka@example.com'),
  (2, 'Maria Garcia', '080-3333-4444', 'maria.garcia@example.com'),
  (3, 'John Smith', '080-5555-6666', 'john.smith@example.com'),
  (4, 'Liu Wei', '080-7777-8888', 'liu.wei@example.com'),
  (5, 'Isabella Rossi', '080-9999-0000', 'isabella.rossi@example.com'),
  (6, 'Emily Chen', '081-1234-5678', 'emily.chen@example.com'),
  (7, 'Ravi Patel', '081-2345-6789', 'ravi.patel@example.com'),
  (8, 'Sophia Lee', '081-3456-7890', 'sophia.lee@example.com'),
  (9, 'Ahmed Hassan', '081-4567-8901', 'ahmed.hassan@example.com'),
  (10, 'Olivia Johnson', '081-5678-9012', 'olivia.johnson@example.com');

-- Inserting example data into the orders table
INSERT INTO orders (order_id, customer_id, order_date, total_price) VALUES
  (1, 1, '2023-11-15', 15.99),
  (2, 2, '2023-11-15', 23.97),
  (3, 3, '2023-11-14', 11.98),
  (4, 4, '2023-11-13', 7.99),
  (5, 5, '2023-11-12', 9.99),
  (6, 1, '2023-11-16', 8.99),
  (7, 2, '2023-11-16', 13.99),
  (8, 6, '2023-11-15', 18.99),
  (9, 7, '2023-11-14', 12.99),
  (10, 8, '2023-11-13', 10.99);

-- Inserting example data into the menu table
INSERT INTO menu (pizza_id, toppings, price) VALUES
  (1, 'Margherita', 3.99),
  (2, 'Pepperoni Lover''s', 3.99), -- Fixed the escaping here
  (3, 'Veggie Supreme', 3.99),
  (4, 'Pesto and Tomato', 4.99),
  (5, 'Mediterranean', 4.99),
  (6, 'BBQ Chicken', 5.99),
  (7, 'Ham&Crab sticks', 5.99),
  (8, 'Hawaiian', 5.99),
  (9, 'Meat Lover''s', 5.99), -- And here
  (10, 'Four Cheese', 5.99),
  (11, 'Tom Yum Kung', 5.99),
  (12, 'Seafood', 7.99),
  (13, 'Truffle Delight', 9.99),
  (14, 'Spicy Tuna', 8.99),
  (15, 'Buffalo Chicken', 7.99),
  (16, 'Greek Special', 6.99),
  (17, 'Four Seasons', 10.99);

.mode box
SELECT * FROM customers;
SELECT * FROM orders;
SELECT * FROM menu;

-- JOIN
SELECT
  customers.customer_id,
  customers.name,
  orders.order_date,
  orders.total_price
FROM customers
JOIN orders ON customers.customer_id = orders.customer_id;

-- Using Subqueries or WITH Clause
WITH AverageOrderPrice AS (
  SELECT AVG(total_price) AS avg_price FROM orders
)
SELECT c.customer_id, c.name, SUM(o.total_price) AS total_spent
FROM customers c
JOIN orders o ON c.customer_id = o.customer_id
GROUP BY c.customer_id, c.name
HAVING SUM(o.total_price) > (SELECT avg_price FROM AverageOrderPrice);

-- Using Aggregate Functions
SELECT
  customers.customer_id,
  customers.name,
  orders.order_date,
  orders.total_price
FROM customers
JOIN orders ON customers.customer_id = orders.customer_id
WHERE orders.total_price > (
  SELECT AVG(total_price)
  FROM orders
);
