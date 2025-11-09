#!/bin/bash

# WOO Dashboard + DMS Complete System Startup Script
# Starts all services in the correct order with health checks

set -e  # Exit on error

# Color codes
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m' # No Color

echo -e "${BLUE}🚀 WOO Dashboard + DMS System Startup${NC}"
echo "========================================"
echo ""

# Function to wait for HTTP endpoint
wait_for_http() {
    local name=$1
    local url=$2
    local max_attempts=${3:-30}
    local attempt=0

    echo -n "Waiting for $name..."

    while [ $attempt -lt $max_attempts ]; do
        if curl -s -o /dev/null -w "%{http_code}" "$url" 2>/dev/null | grep -q "200"; then
            echo -e " ${GREEN}✓${NC}"
            return 0
        fi
        echo -n "."
        sleep 2
        ((attempt++))
    done

    echo -e " ${RED}✗ Timeout${NC}"
    return 1
}

# Function to wait for port
wait_for_port() {
    local name=$1
    local port=$2
    local max_attempts=${3:-30}
    local attempt=0

    echo -n "Waiting for $name (port $port)..."

    while [ $attempt -lt $max_attempts ]; do
        if nc -z localhost "$port" 2>/dev/null; then
            echo -e " ${GREEN}✓${NC}"
            return 0
        fi
        echo -n "."
        sleep 2
        ((attempt++))
    done

    echo -e " ${RED}✗ Timeout${NC}"
    return 1
}

# Step 1: Start NATS JetStream
echo ""
echo -e "${BLUE}Step 1/5: Starting NATS JetStream${NC}"
echo "-----------------------------------"
cd nats-events
docker-compose up -d
cd ..

wait_for_port "NATS" "4222" 15
wait_for_http "NATS Monitoring" "http://localhost:8222/varz" 15
wait_for_http "Event Consumer API" "http://localhost:3002/health" 30

# Step 2: Check which DMS to start
echo ""
echo -e "${BLUE}Step 2/5: Document Management Systems${NC}"
echo "--------------------------------------"
echo "Which DMS do you want to start?"
echo "  1) Paperless-ngx (lightweight, recommended for testing)"
echo "  2) Alfresco (enterprise, requires 4GB+ RAM)"
echo "  3) Both"
echo "  4) Skip (already running)"
echo ""
read -p "Enter choice [1-4]: " dms_choice

case $dms_choice in
    1)
        echo ""
        echo "Starting Paperless-ngx..."
        cd paperless-ngx-dms
        docker-compose up -d
        cd ..

        echo "Waiting for services to initialize (this may take 2-3 minutes)..."
        wait_for_port "PostgreSQL" "5433" 60
        wait_for_http "MinIO" "http://localhost:9000/minio/health/live" 30
        wait_for_http "Paperless API" "http://localhost:8000/api/" 90

        echo -e "${GREEN}✓ Paperless-ngx is ready!${NC}"
        echo "  - Web UI: http://localhost:8000 (admin / changeme123)"
        echo "  - MinIO Console: http://localhost:9001 (minioadmin / minioadmin)"
        ;;
    2)
        echo ""
        echo -e "${YELLOW}⚠️  Starting Alfresco (requires 4GB+ RAM, ~10 min startup)${NC}"
        read -p "Continue? [y/N]: " confirm
        if [[ $confirm =~ ^[Yy]$ ]]; then
            cd alfresco-dms
            docker-compose up -d
            cd ..

            echo "Waiting for services to initialize (this will take ~10 minutes)..."
            wait_for_port "PostgreSQL" "5434" 60
            wait_for_http "MinIO" "http://localhost:9002/minio/health/live" 30
            echo "Waiting for Alfresco Repository (this takes time)..."
            wait_for_http "Alfresco" "http://localhost:8080/alfresco" 300

            echo -e "${GREEN}✓ Alfresco is ready!${NC}"
            echo "  - Share UI: http://localhost:8080/share (admin / admin)"
            echo "  - Digital Workspace: http://localhost:8080/workspace"
            echo "  - MinIO Console: http://localhost:9002 (minioadmin / minioadmin)"
        else
            echo "Skipped Alfresco"
        fi
        ;;
    3)
        echo ""
        echo "Starting both Paperless-ngx and Alfresco..."

        # Start Paperless first
        cd paperless-ngx-dms
        docker-compose up -d
        cd ..

        # Start Alfresco
        cd alfresco-dms
        docker-compose up -d
        cd ..

        echo "Waiting for services (Paperless ~3 min, Alfresco ~10 min)..."
        wait_for_port "Paperless PostgreSQL" "5433" 60
        wait_for_port "Alfresco PostgreSQL" "5434" 60
        wait_for_http "Paperless MinIO" "http://localhost:9000/minio/health/live" 30
        wait_for_http "Alfresco MinIO" "http://localhost:9002/minio/health/live" 30
        wait_for_http "Paperless API" "http://localhost:8000/api/" 90
        wait_for_http "Alfresco" "http://localhost:8080/alfresco" 300

        echo -e "${GREEN}✓ Both systems ready!${NC}"
        ;;
    4)
        echo "Skipping DMS startup"
        ;;
    *)
        echo -e "${RED}Invalid choice, skipping DMS startup${NC}"
        ;;
esac

# Step 3: Configure DMS Simulator
echo ""
echo -e "${BLUE}Step 3/5: Configuring DMS Simulator${NC}"
echo "------------------------------------"

cd dms-simulator

if [ ! -f .env ]; then
    echo "Creating .env file from template..."
    cp .env.example .env

    # Auto-configure based on DMS choice
    case $dms_choice in
        1)
            sed -i '' 's/PAPERLESS_ENABLED=false/PAPERLESS_ENABLED=true/' .env
            sed -i '' 's/ALFRESCO_ENABLED=true/ALFRESCO_ENABLED=false/' .env
            ;;
        2)
            sed -i '' 's/PAPERLESS_ENABLED=true/PAPERLESS_ENABLED=false/' .env
            sed -i '' 's/ALFRESCO_ENABLED=false/ALFRESCO_ENABLED=true/' .env
            ;;
        3)
            sed -i '' 's/PAPERLESS_ENABLED=false/PAPERLESS_ENABLED=true/' .env
            sed -i '' 's/ALFRESCO_ENABLED=false/ALFRESCO_ENABLED=true/' .env
            ;;
    esac

    echo -e "${GREEN}✓ .env configured${NC}"
else
    echo ".env already exists, keeping current configuration"
fi

# Check if dependencies are installed
if [ ! -d "node_modules" ]; then
    echo "Installing DMS Simulator dependencies..."
    npm install
fi

cd ..

# Step 4: Start DMS Simulator in background
echo ""
echo -e "${BLUE}Step 4/5: Starting DMS Simulator${NC}"
echo "---------------------------------"

cd dms-simulator

# Check if already running
if lsof -ti:3001 >/dev/null 2>&1; then
    echo -e "${YELLOW}DMS Simulator already running on port 3001${NC}"
    read -p "Restart? [y/N]: " restart
    if [[ $restart =~ ^[Yy]$ ]]; then
        echo "Stopping existing instance..."
        kill $(lsof -ti:3001) 2>/dev/null || true
        sleep 2
    fi
fi

if ! lsof -ti:3001 >/dev/null 2>&1; then
    echo "Starting DMS Simulator..."
    npm run dev > ../dms-simulator.log 2>&1 &
    DMS_PID=$!
    echo "DMS Simulator PID: $DMS_PID"

    cd ..
    wait_for_http "DMS Simulator" "http://localhost:3001/health" 30
    echo -e "${GREEN}✓ DMS Simulator ready${NC}"
else
    cd ..
    echo -e "${GREEN}✓ DMS Simulator running${NC}"
fi

# Step 5: Start WOO Dashboard
echo ""
echo -e "${BLUE}Step 5/5: Starting WOO Dashboard${NC}"
echo "---------------------------------"

# Check if dependencies are installed
if [ ! -d "node_modules" ]; then
    echo "Installing WOO Dashboard dependencies..."
    npm install
fi

# Check if already running
if lsof -ti:5173 >/dev/null 2>&1; then
    echo -e "${YELLOW}WOO Dashboard already running on port 5173${NC}"
else
    echo "Starting WOO Dashboard..."
    npm run dev > woo-dashboard.log 2>&1 &
    DASHBOARD_PID=$!
    echo "Dashboard PID: $DASHBOARD_PID"

    wait_for_http "WOO Dashboard" "http://localhost:5173" 30
fi

# Final summary
echo ""
echo -e "${GREEN}========================================${NC}"
echo -e "${GREEN}✓ Complete System Started Successfully!${NC}"
echo -e "${GREEN}========================================${NC}"
echo ""
echo -e "${BLUE}📊 Access Points:${NC}"
echo "  • WOO Dashboard:        http://localhost:5173"
echo "  • DMS Simulator API:    http://localhost:3001"
echo "  • NATS Event Consumer:  http://localhost:3002"
echo "  • NATS Monitoring:      http://localhost:8222"

if [[ $dms_choice == "1" || $dms_choice == "3" ]]; then
    echo ""
    echo -e "${BLUE}📄 Paperless-ngx:${NC}"
    echo "  • Web UI:              http://localhost:8000"
    echo "    Login: admin / changeme123"
    echo "  • MinIO Console:       http://localhost:9001"
    echo "    Login: minioadmin / minioadmin"
fi

if [[ $dms_choice == "2" || $dms_choice == "3" ]]; then
    echo ""
    echo -e "${BLUE}🏢 Alfresco:${NC}"
    echo "  • Share UI:            http://localhost:8080/share"
    echo "  • Digital Workspace:   http://localhost:8080/workspace"
    echo "    Login: admin / admin"
    echo "  • MinIO Console:       http://localhost:9002"
    echo "    Login: minioadmin / minioadmin"
fi

echo ""
echo -e "${BLUE}📝 Logs:${NC}"
echo "  • DMS Simulator:       tail -f dms-simulator.log"
echo "  • WOO Dashboard:       tail -f woo-dashboard.log"
echo "  • NATS:                docker-compose -f nats-events/docker-compose.yml logs -f"
if [[ $dms_choice == "1" || $dms_choice == "3" ]]; then
    echo "  • Paperless:           docker-compose -f paperless-ngx-dms/docker-compose.yml logs -f"
fi
if [[ $dms_choice == "2" || $dms_choice == "3" ]]; then
    echo "  • Alfresco:            docker-compose -f alfresco-dms/docker-compose.yml logs -f"
fi

echo ""
echo -e "${BLUE}🔍 Verification:${NC}"
echo "  Run: ./verify-system.sh"
echo ""
echo -e "${BLUE}📚 Documentation:${NC}"
echo "  • Complete Guide:      COMPLETE_SYSTEM_GUIDE.md"
echo "  • NATS Quick Start:    nats-events/QUICKSTART.md"
echo ""
echo -e "${GREEN}Happy testing! 🚀${NC}"
