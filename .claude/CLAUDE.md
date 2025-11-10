# WOO Dashboard - Project Context & Memory

## Project Overview
WOO Dashboard is an interactive monitoring system for Wet Open Overheid (Dutch Freedom of Information Act) requests from Gemeente Utrecht and Provincie Flevoland. The project features a modern React-based dashboard with multiple backend options and comprehensive document management integration.

## Architecture

### Frontend Stack
- **Framework**: React 18 with TypeScript
- **Build Tool**: Vite 5.x
- **Visualization**: Recharts for charts and graphs
- **State Management**: React hooks (useState, useEffect)
- **Styling**: CSS3 with custom properties and responsive grid

### Backend Options (Triple Architecture)
1. **Mock Backend**: In-memory TypeScript/JavaScript database
   - Location: `src/lib/mockDatabase.ts`
   - 24 pre-loaded sample documents
   - Client-side only, no server required

2. **Erlang Backend**: Production-grade OTP application
   - Location: `erlang-backend/`
   - Cowboy web server
   - Gen_server for document management
   - Gen_event for notifications
   - Supervisor tree for fault tolerance
   - ETS for in-memory storage

3. **PostgreSQL Backend**: Modern database with advanced features
   - Location: `postgres-backend/`
   - PostgreSQL 14+ with JSONB, LISTEN/NOTIFY
   - Node.js/Fastify REST API
   - Full-text search with tsvector
   - Materialized views for aggregations
   - Trigger-based audit trails

### Document Management Systems
- **Paperless-ngx**: Lightweight open-source DMS with OCR
  - Location: `paperless-ngx-dms/`
  - PostgreSQL 18 backend
  - MinIO S3-compatible storage (LOCAL, not AWS cloud!)

- **Alfresco**: Enterprise ECM platform
  - Location: `alfresco-dms/`
  - Digital Workspace and Share UI
  - Workflow management

- **DMS Simulator**: Interactive document generator
  - Location: `dms-simulator/`
  - Generates 6 types of realistic PDFs
  - Batch upload (1-50 documents)

### Event Streaming
- **NATS JetStream**: Persistent event streaming
  - Location: `nats-events/`
  - 7-day retention policy
  - Event Consumer API on port 3002
  - SSE support for real-time updates

## Key File Locations

### Core Application
- `src/App.tsx` - Main application component
- `src/components/` - React components
- `src/lib/mockDatabase.ts` - Mock backend implementation
- `src/types/` - TypeScript type definitions

### Configuration
- `vite.config.ts` - Vite build configuration
- `tsconfig.json` - TypeScript compiler settings
- `.env.example` - Environment variables template

### Deployment
- `index.html` - Entry HTML file
- `dist/` - Production build output
- `.github/workflows/` - GitHub Actions CI/CD

### Utilities
- `start-all.sh` - Automated startup script for all services
- `verify-system.sh` - Health check verification tool
- `COMPLETE_SYSTEM_GUIDE.md` - Comprehensive documentation

## Workflow & Processes

### WOO Document Status Flow (6 Stages)
1. **Ontvangen** (Received)
2. **In behandeling** (In Progress)
3. **1e Concept** (First Draft)
4. **2e Concept** (Second Draft)
5. **Definitief** (Final)
6. **Gepubliceerd** (Published)

- Linear progression with cyclical restart
- 2-second interval for simulation
- Real-time event feed for all status changes

### Development Workflow
1. Start services: `./start-all.sh`
2. Verify health: `./verify-system.sh`
3. Development server: `npm run dev` (port 5173)
4. Build for production: `npm run build`
5. Deploy to GitHub Pages: `npm run deploy`

## Current Focus Areas

### Active Development
- Multi-backend architecture switching via UI
- Real-time event streaming integration
- Document management system coordination
- PostgreSQL 18 advanced features utilization

### Recent Decisions
- Chose MinIO for local S3-compatible storage (NOT AWS cloud)
- Implemented NATS JetStream for event persistence
- Added DMS Simulator for realistic document generation
- Created unified startup and verification scripts

### Known Issues & Considerations
- GitHub Pages CDN caching: 10-60 minutes for updates
- Use incognito mode or hard refresh for immediate testing
- Docker Compose manages all backend services
- PostgreSQL 18 provides modern database features

## Dependencies & Integration Points

### External Services (Docker Compose)
- PostgreSQL 18 (port 5432)
- MinIO S3 storage (port 9000)
- NATS JetStream (port 4222)
- Paperless-ngx web UI (port 8000)
- Alfresco web UI (port 8080)

### NPM Packages
- react@18.2.0
- react-dom@18.2.0
- recharts@2.10.3
- sql.js@1.13.0
- TypeScript@5.3.3
- Vite@5.0.8

## Conventions & Best Practices

### Code Style
- TypeScript strict mode enabled
- React functional components with hooks
- CSS custom properties for theming
- Responsive design patterns

### Git Workflow
- Main branch: production deployments
- Feature branches for development
- GitHub Actions for automated deployments
- gh-pages branch for live demo

### Documentation
- Comprehensive README.md
- Inline code comments for complex logic
- System guide for operational details
- API documentation for backend endpoints

## Memory & Context Notes

### Important Patterns
- Actor model simulation in TypeScript mock backend
- Erlang-inspired event handling across all backends
- Unified document interface across DMS systems
- Event-driven architecture for real-time updates

### Performance Considerations
- In-memory caching for frequently accessed data
- Materialized views for expensive aggregations
- Client-side filtering and sorting where possible
- Lazy loading for large document lists

### Security & Permissions
- Local-only development environment
- No external cloud dependencies
- Docker network isolation for services
- Environment variables for sensitive configuration

## Quick Reference Commands

```bash
# Start all services
./start-all.sh

# Verify system health
./verify-system.sh

# Development
npm run dev

# Production build
npm run build

# Deploy to GitHub Pages
npm run deploy
```

## Project Goals
1. Provide transparent WOO request monitoring
2. Demonstrate multiple backend architecture patterns
3. Integrate comprehensive document management
4. Enable real-time event streaming and notifications
5. Maintain production-quality code standards
6. Showcase modern TypeScript and React practices

---

**Last Updated**: Context initialization
**Project Status**: Active development
**Deployment**: https://terminal-woo.github.io/woo-dashboard/
