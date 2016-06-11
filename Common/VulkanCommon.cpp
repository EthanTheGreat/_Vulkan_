// Copyright 2016 Intel Corporation All Rights Reserved
// 
// Intel makes no representations about the suitability of this software for any purpose.
// THIS SOFTWARE IS PROVIDED ""AS IS."" INTEL SPECIFICALLY DISCLAIMS ALL WARRANTIES,
// EXPRESS OR IMPLIED, AND ALL LIABILITY, INCLUDING CONSEQUENTIAL AND OTHER INDIRECT DAMAGES,
// FOR THE USE OF THIS SOFTWARE, INCLUDING LIABILITY FOR INFRINGEMENT OF ANY PROPRIETARY
// RIGHTS, AND INCLUDING THE WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
// Intel does not assume any responsibility for any errors which may appear in this software
// nor any responsibility to update it.

#include "VulkanCommon.h"
#include "VulkanFunctions.h"

namespace ApiWithoutSecrets {

  VulkanCommon::VulkanCommon() :
    VulkanLibrary(),
    //Window(),
    Vulkan() {
  }

  void VulkanCommon::PrepareGLFWHooks( ) {
	  VulkanCommon* glfwHandlePoint = this;

	  glfwSetInputMode(Window, GLFW_CURSOR, GLFW_CURSOR_NORMAL);
	  glfwSetWindowUserPointer(Window, glfwHandlePoint);
	  glfwSetMouseButtonCallback(Window, [](GLFWwindow* window, int key, int action, int mods) {

		  if (key == GLFW_MOUSE_BUTTON_1 && action == GLFW_PRESS)
			  glfwSetInputMode(window, GLFW_CURSOR, GLFW_CURSOR_DISABLED);

		  if (key == GLFW_MOUSE_BUTTON_1 && action == GLFW_RELEASE)
			  glfwSetInputMode(window, GLFW_CURSOR, GLFW_CURSOR_NORMAL);
	  });
	  glfwSetWindowRefreshCallback(Window, [](GLFWwindow* window) {
		  auto* obj = static_cast<VulkanCommon*>(glfwGetWindowUserPointer(window));

		  if (obj == nullptr) return;

		  obj->Draw();
	  });
	  glfwSetFramebufferSizeCallback(Window, [](GLFWwindow* window, int width, int height) {
		  auto* obj = static_cast<VulkanCommon*>(glfwGetWindowUserPointer(window));

		  if (obj == nullptr) return;

		  obj->OnWindowSizeChanged();

		  return;
	  });
	  glfwSetKeyCallback(Window, [](GLFWwindow* window, int key, int scancode, int action, int mods) {
		  auto* obj = static_cast<VulkanCommon*>(glfwGetWindowUserPointer(window));

		  if (obj == nullptr) return;

		  if (key == GLFW_KEY_ESCAPE && action == GLFW_RELEASE) {
			  obj->~VulkanCommon();
			  exit(0);
		  }

		  const float Camera_Speed = 1.5f;

		  if (key == GLFW_KEY_W && (action == GLFW_REPEAT || action == GLFW_PRESS)) { obj->Camera.origin += obj->Camera.viewDir * Camera_Speed; }
		  if (key == GLFW_KEY_S && (action == GLFW_REPEAT || action == GLFW_PRESS)) { obj->Camera.origin -= obj->Camera.viewDir * Camera_Speed; }

		  if (key == GLFW_KEY_A && (action == GLFW_REPEAT || action == GLFW_PRESS)) { obj->Camera.origin -= obj->Camera.movDir * Camera_Speed; };
		  if (key == GLFW_KEY_D && (action == GLFW_REPEAT || action == GLFW_PRESS)) { obj->Camera.origin += obj->Camera.movDir * Camera_Speed; };

		  if (key == GLFW_KEY_R && (action == GLFW_REPEAT || action == GLFW_PRESS)) { obj->Camera.origin += obj->Camera.up * Camera_Speed; };
		  if (key == GLFW_KEY_F && (action == GLFW_REPEAT || action == GLFW_PRESS)) { obj->Camera.origin -= obj->Camera.up * Camera_Speed; };
	  });
	  glfwSetCursorPosCallback(Window, [](GLFWwindow* window, double xPos, double yPos) {

		  if (glfwGetInputMode(window, GLFW_CURSOR) != GLFW_CURSOR_DISABLED)
			  return;
		  auto* obj = static_cast<VulkanCommon*>(glfwGetWindowUserPointer(window));

		  if (obj == nullptr) return;

		  glm::vec2 curMousePos(xPos, yPos);

		  glm::vec2 mouseDelta = (curMousePos - obj->Camera.oldMousePos);

		  if (glm::length(mouseDelta) > 100.0f) {
			  obj->Camera.oldMousePos = curMousePos;
			  return;
		  }
		  const float ROTATION_SPEED = 0.01f;

		  obj->Camera.movDir = glm::cross(obj->Camera.viewDir, obj->Camera.up);
		  glm::mat4 rotator = glm::rotate(-mouseDelta.x * ROTATION_SPEED, obj->Camera.up)
			  * glm::rotate(-mouseDelta.y * ROTATION_SPEED, obj->Camera.movDir);

		  obj->Camera.viewDir = glm::mat3(rotator) * obj->Camera.viewDir;
		  obj->Camera.oldMousePos = curMousePos;
	  });
	  glfwSetWindowCloseCallback(Window, [](GLFWwindow* window) {
		  auto* obj = static_cast<VulkanCommon*>(glfwGetWindowUserPointer(window));

		  if (obj == nullptr) return;

		  obj->~VulkanCommon();
		  exit(0);
	  });
  }

  bool VulkanCommon::PrepareVulkan( GLFWwindow* mainWindow_ ) {
    Window = mainWindow_;

	PrepareGLFWHooks();

    if( !LoadVulkanLibrary() ) {
      return false;
    }
    if( !LoadExportedEntryPoints() ) {
      return false;
    }
    if( !LoadGlobalLevelEntryPoints() ) {
      return false;
    }
    if( !CreateInstance() ) {
      return false;
    }
	if( !LoadInstanceLevelEntryPoints() ) {
      return false;
    }
	if( !CreatePresentationSurface() ) {
      return false;
    }
    if( !CreateDevice() ) {
      return false;
    }
    if( !LoadDeviceLevelEntryPoints() ) {
      return false;
    }
    if( !GetDeviceQueue() ) {
      return false;
    }
    if( !CreateSwapChain() ) {
      return false;
    }
    return true;
  }

  bool VulkanCommon::OnWindowSizeChanged() {
    ChildClear();

    if( !CreateSwapChain() ) {
      return false;
    }

	vkDestroyImageView(GetDevice(), Depth.View, nullptr);
	vkDestroyImage(GetDevice(), Depth.Image, nullptr);
	vkFreeMemory(GetDevice(), Depth.Mem, nullptr);
	//PrepareDepthBuffer(CommandBuffer_);

    return ChildOnWindowSizeChanged();
  }

  VkPhysicalDevice VulkanCommon::GetPhysicalDevice() const {
    return Vulkan.PhysicalDevice;
  }

  VkDevice VulkanCommon::GetDevice() const {
    return Vulkan.Device;
  }

  const QueueParameters VulkanCommon::GetGraphicsQueue() const {
    return Vulkan.GraphicsQueue;
  }

  const QueueParameters VulkanCommon::GetPresentQueue() const {
    return Vulkan.PresentQueue;
  }

  const SwapChainParameters VulkanCommon::GetSwapChain() const {
    return Vulkan.SwapChain;
  }

  bool VulkanCommon::LoadVulkanLibrary() {
#if defined(VK_USE_PLATFORM_WIN32_KHR)
    VulkanLibrary = LoadLibrary( "vulkan-1.dll" );
#elif defined(VK_USE_PLATFORM_XCB_KHR) || defined(VK_USE_PLATFORM_XLIB_KHR)
    VulkanLibrary = dlopen( "libvulkan.so", RTLD_NOW );
#endif

    if( VulkanLibrary == nullptr ) {
      std::cout << "Could not load Vulkan library!" << std::endl;
      return false;
    }
    return true;
  }

  bool VulkanCommon::LoadExportedEntryPoints() {
#if defined(VK_USE_PLATFORM_WIN32_KHR)
    #define LoadProcAddress GetProcAddress
#elif defined(VK_USE_PLATFORM_XCB_KHR) || defined(VK_USE_PLATFORM_XLIB_KHR)
    #define LoadProcAddress dlsym
#endif

#define VK_EXPORTED_FUNCTION( fun )                                                   \
    if( !(fun = (PFN_##fun)LoadProcAddress( VulkanLibrary, #fun )) ) {                \
      std::cout << "Could not load exported function: " << #fun << "!" << std::endl;  \
      return false;                                                                   \
    }

#include "ListOfFunctions.inl"

    return true;
  }

  bool VulkanCommon::LoadGlobalLevelEntryPoints() {
#define VK_GLOBAL_LEVEL_FUNCTION( fun )                                                   \
    if( !(fun = (PFN_##fun)vkGetInstanceProcAddr( nullptr, #fun )) ) {                    \
      std::cout << "Could not load global level function: " << #fun << "!" << std::endl;  \
      return false;                                                                       \
    }

#include "ListOfFunctions.inl"

      return true;
  }

  bool VulkanCommon::CreateInstance() {
    uint32_t extensions_count = 0;
    if( (vkEnumerateInstanceExtensionProperties( nullptr, &extensions_count, nullptr ) != VK_SUCCESS) ||
        (extensions_count == 0) ) {
      std::cout << "Error occurred during instance extensions enumeration!" << std::endl;
      return false;
    }

    std::vector<VkExtensionProperties> available_extensions( extensions_count );
    if( vkEnumerateInstanceExtensionProperties( nullptr, &extensions_count, &available_extensions[0] ) != VK_SUCCESS ) {
      std::cout << "Error occurred during instance extensions enumeration!" << std::endl;
      return false;
    }

    std::vector<const char*> extensions = {
      VK_KHR_SURFACE_EXTENSION_NAME,
#if defined(VK_USE_PLATFORM_WIN32_KHR)
      VK_KHR_WIN32_SURFACE_EXTENSION_NAME
#elif defined(VK_USE_PLATFORM_XCB_KHR)
      VK_KHR_XCB_SURFACE_EXTENSION_NAME
#elif defined(VK_USE_PLATFORM_XLIB_KHR)
      VK_KHR_XLIB_SURFACE_EXTENSION_NAME
#endif
    };

    for( size_t i = 0; i < extensions.size(); ++i ) {
      if( !CheckExtensionAvailability( extensions[i], available_extensions ) ) {
        std::cout << "Could not find instance extension named \"" << extensions[i] << "\"!" << std::endl;
        return false;
      }
    }

    VkApplicationInfo application_info = {
      VK_STRUCTURE_TYPE_APPLICATION_INFO,             // VkStructureType            sType
      nullptr,                                        // const void                *pNext
      "API without Secrets: Introduction to Vulkan",  // const char                *pApplicationName
      VK_MAKE_VERSION( 1, 0, 0 ),                     // uint32_t                   applicationVersion
      "Vulkan Tutorial by Intel",                     // const char                *pEngineName
      VK_MAKE_VERSION( 1, 0, 0 ),                     // uint32_t                   engineVersion
      VK_API_VERSION                                  // uint32_t                   apiVersion
    };

    VkInstanceCreateInfo instance_create_info = {
      VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO,         // VkStructureType            sType
      nullptr,                                        // const void                *pNext
      0,                                              // VkInstanceCreateFlags      flags
      &application_info,                              // const VkApplicationInfo   *pApplicationInfo
      0,                                              // uint32_t                   enabledLayerCount
      nullptr,                                        // const char * const        *ppEnabledLayerNames
      static_cast<uint32_t>(extensions.size()),       // uint32_t                   enabledExtensionCount
      &extensions[0]                                  // const char * const        *ppEnabledExtensionNames
    };

    if( vkCreateInstance( &instance_create_info, nullptr, &Vulkan.Instance ) != VK_SUCCESS ) {
      std::cout << "Could not create Vulkan instance!" << std::endl;
      return false;
    }
    return true;
  }

  bool VulkanCommon::LoadInstanceLevelEntryPoints() {
#define VK_INSTANCE_LEVEL_FUNCTION( fun )                                                   \
    if( !(fun = (PFN_##fun)vkGetInstanceProcAddr( Vulkan.Instance, #fun )) ) {              \
      std::cout << "Could not load instance level function: " << #fun << "!" << std::endl;  \
      return false;                                                                         \
    }

#include "ListOfFunctions.inl"

      return true;
  }

  bool VulkanCommon::CreatePresentationSurface() {
#if defined(VK_USE_PLATFORM_WIN32_KHR)
	  VkResult err = glfwCreateWindowSurface(Vulkan.Instance, Window, NULL, &Vulkan.PresentationSurface);

	  if (err != VK_SUCCESS) {
		  std::cout << "GLFW, Surface didn't load correctly." << std::endl;
		  system("PAUSE");
	  }
	  else {
		  return true;
	  }

#elif defined(VK_USE_PLATFORM_XCB_KHR)
    VkXcbSurfaceCreateInfoKHR surface_create_info = {
      VK_STRUCTURE_TYPE_XCB_SURFACE_CREATE_INFO_KHR,    // VkStructureType                  sType
      nullptr,                                          // const void                      *pNext
      0,                                                // VkXcbSurfaceCreateFlagsKHR       flags
      Window.Connection,                                // xcb_connection_t*                connection
      Window.Handle                                     // xcb_window_t                     window
    };

    if( vkCreateXcbSurfaceKHR( Vulkan.Instance, &surface_create_info, nullptr, &Vulkan.PresentationSurface ) == VK_SUCCESS ) {
      return true;
    }

#elif defined(VK_USE_PLATFORM_XLIB_KHR)
    VkXlibSurfaceCreateInfoKHR surface_create_info = {
      VK_STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR,   // VkStructureType                sType
      nullptr,                                          // const void                    *pNext
      0,                                                // VkXlibSurfaceCreateFlagsKHR    flags
      Window.DisplayPtr,                                // Display                       *dpy
      Window.Handle                                     // Window                         window
    };
    if( vkCreateXlibSurfaceKHR( Vulkan.Instance, &surface_create_info, nullptr, &Vulkan.PresentationSurface ) == VK_SUCCESS ) {
      return true;
    }

#endif

    std::cout << "Could not create presentation surface!" << std::endl;
    return false;
  }

  bool VulkanCommon::CreateDevice() {
    uint32_t num_devices = 0;
    if( (vkEnumeratePhysicalDevices( Vulkan.Instance, &num_devices, nullptr ) != VK_SUCCESS) ||
        (num_devices == 0) ) {
      std::cout << "Error occurred during physical devices enumeration!" << std::endl;
      return false;
    }

    std::vector<VkPhysicalDevice> physical_devices( num_devices );
    if( vkEnumeratePhysicalDevices( Vulkan.Instance, &num_devices, &physical_devices[0] ) != VK_SUCCESS ) {
      std::cout << "Error occurred during physical devices enumeration!" << std::endl;
      return false;
    }

    uint32_t selected_graphics_queue_family_index = UINT32_MAX;
    uint32_t selected_present_queue_family_index = UINT32_MAX;

    for( uint32_t i = 0; i < num_devices; ++i ) {
      if( CheckPhysicalDeviceProperties( physical_devices[i], selected_graphics_queue_family_index, selected_present_queue_family_index ) ) {
        Vulkan.PhysicalDevice = physical_devices[i];
      }
    }
    if( Vulkan.PhysicalDevice == VK_NULL_HANDLE ) {
      std::cout << "Could not select physical device based on the chosen properties!" << std::endl;
      return false;
    }

    std::vector<VkDeviceQueueCreateInfo> queue_create_infos;
    std::vector<float> queue_priorities = { 1.0f };

    queue_create_infos.push_back( {
      VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO,       // VkStructureType              sType
      nullptr,                                          // const void                  *pNext
      0,                                                // VkDeviceQueueCreateFlags     flags
      selected_graphics_queue_family_index,             // uint32_t                     queueFamilyIndex
      static_cast<uint32_t>(queue_priorities.size()),   // uint32_t                     queueCount
      &queue_priorities[0]                              // const float                 *pQueuePriorities
    } );

    if( selected_graphics_queue_family_index != selected_present_queue_family_index ) {
      queue_create_infos.push_back( {
        VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO,     // VkStructureType              sType
        nullptr,                                        // const void                  *pNext
        0,                                              // VkDeviceQueueCreateFlags     flags
        selected_present_queue_family_index,            // uint32_t                     queueFamilyIndex
        static_cast<uint32_t>(queue_priorities.size()), // uint32_t                     queueCount
        &queue_priorities[0]                            // const float                 *pQueuePriorities
      } );
    }

    std::vector<const char*> extensions = {
      VK_KHR_SWAPCHAIN_EXTENSION_NAME
    };

    VkDeviceCreateInfo device_create_info = {
      VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO,             // VkStructureType                    sType
      nullptr,                                          // const void                        *pNext
      0,                                                // VkDeviceCreateFlags                flags
      static_cast<uint32_t>(queue_create_infos.size()), // uint32_t                           queueCreateInfoCount
      &queue_create_infos[0],                           // const VkDeviceQueueCreateInfo     *pQueueCreateInfos
      0,                                                // uint32_t                           enabledLayerCount
      nullptr,                                          // const char * const                *ppEnabledLayerNames
      static_cast<uint32_t>(extensions.size()),         // uint32_t                           enabledExtensionCount
      &extensions[0],                                   // const char * const                *ppEnabledExtensionNames
      nullptr                                           // const VkPhysicalDeviceFeatures    *pEnabledFeatures
    };

    if( vkCreateDevice( Vulkan.PhysicalDevice, &device_create_info, nullptr, &Vulkan.Device ) != VK_SUCCESS ) {
      std::cout << "Could not create Vulkan device!" << std::endl;
      return false;
    }

    Vulkan.GraphicsQueue.FamilyIndex = selected_graphics_queue_family_index;
    Vulkan.PresentQueue.FamilyIndex = selected_present_queue_family_index;
    return true;
  }

  bool VulkanCommon::CheckPhysicalDeviceProperties( VkPhysicalDevice physical_device, uint32_t &selected_graphics_queue_family_index, uint32_t &selected_present_queue_family_index ) {
    uint32_t extensions_count = 0;
    if( (vkEnumerateDeviceExtensionProperties( physical_device, nullptr, &extensions_count, nullptr ) != VK_SUCCESS) ||
        (extensions_count == 0) ) {
      std::cout << "Error occurred during physical device " << physical_device << " extensions enumeration!" << std::endl;
      return false;
    }

    std::vector<VkExtensionProperties> available_extensions( extensions_count );
    if( vkEnumerateDeviceExtensionProperties( physical_device, nullptr, &extensions_count, &available_extensions[0] ) != VK_SUCCESS ) {
      std::cout << "Error occurred during physical device " << physical_device << " extensions enumeration!" << std::endl;
      return false;
    }

    std::vector<const char*> device_extensions = {
      VK_KHR_SWAPCHAIN_EXTENSION_NAME
    };

    for( size_t i = 0; i < device_extensions.size(); ++i ) {
      if( !CheckExtensionAvailability( device_extensions[i], available_extensions ) ) {
        std::cout << "Physical device " << physical_device << " doesn't support extension named \"" << device_extensions[i] << "\"!" << std::endl;
        return false;
      }
    }

    VkPhysicalDeviceProperties device_properties;
    VkPhysicalDeviceFeatures   device_features;

    vkGetPhysicalDeviceProperties( physical_device, &device_properties );
    vkGetPhysicalDeviceFeatures( physical_device, &device_features );

    uint32_t major_version = VK_VERSION_MAJOR( device_properties.apiVersion );

    if( (major_version < 1) &&
        (device_properties.limits.maxImageDimension2D < 4096) ) {
      std::cout << "Physical device " << physical_device << " doesn't support required parameters!" << std::endl;
      return false;
    }

    uint32_t queue_families_count = 0;
    vkGetPhysicalDeviceQueueFamilyProperties( physical_device, &queue_families_count, nullptr );
    if( queue_families_count == 0 ) {
      std::cout << "Physical device " << physical_device << " doesn't have any queue families!" << std::endl;
      return false;
    }

    std::vector<VkQueueFamilyProperties>  queue_family_properties( queue_families_count );
    std::vector<VkBool32>                 queue_present_support( queue_families_count );

    vkGetPhysicalDeviceQueueFamilyProperties( physical_device, &queue_families_count, &queue_family_properties[0] );

    uint32_t graphics_queue_family_index = UINT32_MAX;
    uint32_t present_queue_family_index = UINT32_MAX;

    for( uint32_t i = 0; i < queue_families_count; ++i ) {
      vkGetPhysicalDeviceSurfaceSupportKHR( physical_device, i, Vulkan.PresentationSurface, &queue_present_support[i] );

      if( (queue_family_properties[i].queueCount > 0) &&
          (queue_family_properties[i].queueFlags & VK_QUEUE_GRAPHICS_BIT) ) {
        // Select first queue that supports graphics
        if( graphics_queue_family_index == UINT32_MAX ) {
          graphics_queue_family_index = i;
        }

        // If there is queue that supports both graphics and present - prefer it
        if( queue_present_support[i] ) {
          selected_graphics_queue_family_index = i;
          selected_present_queue_family_index = i;
          return true;
        }
      }
    }

    // We don't have queue that supports both graphics and present so we have to use separate queues
    for( uint32_t i = 0; i < queue_families_count; ++i ) {
      if( queue_present_support[i] ) {
        present_queue_family_index = i;
        break;
      }
    }

    // If this device doesn't support queues with graphics and present capabilities don't use it
    if( (graphics_queue_family_index == UINT32_MAX) ||
        (present_queue_family_index == UINT32_MAX) ) {
      std::cout << "Could not find queue families with required properties on physical device " << physical_device << "!" << std::endl;
      return false;
    }

    selected_graphics_queue_family_index = graphics_queue_family_index;
    selected_present_queue_family_index = present_queue_family_index;
    return true;
  }

  bool VulkanCommon::LoadDeviceLevelEntryPoints() {
#define VK_DEVICE_LEVEL_FUNCTION( fun )                                                   \
    if( !(fun = (PFN_##fun)vkGetDeviceProcAddr( Vulkan.Device, #fun )) ) {                \
      std::cout << "Could not load device level function: " << #fun << "!" << std::endl;  \
      return false;                                                                       \
    }

#include "ListOfFunctions.inl"

      return true;
  }

  bool VulkanCommon::GetDeviceQueue() {
    vkGetDeviceQueue( Vulkan.Device, Vulkan.GraphicsQueue.FamilyIndex, 0, &Vulkan.GraphicsQueue.Handle );
    vkGetDeviceQueue( Vulkan.Device, Vulkan.PresentQueue.FamilyIndex, 0, &Vulkan.PresentQueue.Handle );
    return true;
  }

  void VulkanCommon::setImageLayout(
	  VkCommandBuffer cmdbuffer,
	  VkImage image,
	  VkImageAspectFlags aspectMask,
	  VkImageLayout oldImageLayout,
	  VkImageLayout newImageLayout,
	  VkImageSubresourceRange subresourceRange)
  {
	  // Create an image barrier object
	  VkImageMemoryBarrier imageMemoryBarrier = {};
	  imageMemoryBarrier.sType = VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER;
	  imageMemoryBarrier.pNext = NULL;
	  imageMemoryBarrier.srcQueueFamilyIndex = VK_QUEUE_FAMILY_IGNORED;
	  imageMemoryBarrier.dstQueueFamilyIndex = VK_QUEUE_FAMILY_IGNORED;
	  imageMemoryBarrier.oldLayout = oldImageLayout;
	  imageMemoryBarrier.newLayout = newImageLayout;
	  imageMemoryBarrier.image = image;
	  imageMemoryBarrier.subresourceRange = subresourceRange;

	  // Source layouts (old)

	  // Undefined layout
	  // Only allowed as initial layout!
	  // Make sure any writes to the image have been finished
	  if (oldImageLayout == VK_IMAGE_LAYOUT_PREINITIALIZED)
	  {
		  imageMemoryBarrier.srcAccessMask = VK_ACCESS_HOST_WRITE_BIT | VK_ACCESS_TRANSFER_WRITE_BIT;
	  }

	  // Old layout is color attachment
	  // Make sure any writes to the color buffer have been finished
	  if (oldImageLayout == VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL)
	  {
		  imageMemoryBarrier.srcAccessMask = VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT;
	  }

	  // Old layout is depth/stencil attachment
	  // Make sure any writes to the depth/stencil buffer have been finished
	  if (oldImageLayout == VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL)
	  {
		  imageMemoryBarrier.srcAccessMask = VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT;
	  }

	  // Old layout is transfer source
	  // Make sure any reads from the image have been finished
	  if (oldImageLayout == VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL)
	  {
		  imageMemoryBarrier.srcAccessMask = VK_ACCESS_TRANSFER_READ_BIT;
	  }

	  // Old layout is shader read (sampler, input attachment)
	  // Make sure any shader reads from the image have been finished
	  if (oldImageLayout == VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)
	  {
		  imageMemoryBarrier.srcAccessMask = VK_ACCESS_SHADER_READ_BIT;
	  }

	  // Target layouts (new)

	  // New layout is transfer destination (copy, blit)
	  // Make sure any copyies to the image have been finished
	  if (newImageLayout == VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL)
	  {
		  imageMemoryBarrier.dstAccessMask = VK_ACCESS_TRANSFER_WRITE_BIT;
	  }

	  // New layout is transfer source (copy, blit)
	  // Make sure any reads from and writes to the image have been finished
	  if (newImageLayout == VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL)
	  {
		  imageMemoryBarrier.srcAccessMask = imageMemoryBarrier.srcAccessMask | VK_ACCESS_TRANSFER_READ_BIT;
		  imageMemoryBarrier.dstAccessMask = VK_ACCESS_TRANSFER_READ_BIT;
	  }

	  // New layout is color attachment
	  // Make sure any writes to the color buffer hav been finished
	  if (newImageLayout == VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL)
	  {
		  imageMemoryBarrier.dstAccessMask = VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT;
		  imageMemoryBarrier.srcAccessMask = VK_ACCESS_TRANSFER_READ_BIT;
	  }

	  // New layout is depth attachment
	  // Make sure any writes to depth/stencil buffer have been finished
	  if (newImageLayout == VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL)
	  {
		  imageMemoryBarrier.dstAccessMask = imageMemoryBarrier.dstAccessMask | VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT;
	  }

	  // New layout is shader read (sampler, input attachment)
	  // Make sure any writes to the image have been finished
	  if (newImageLayout == VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)
	  {
		  imageMemoryBarrier.srcAccessMask = VK_ACCESS_HOST_WRITE_BIT | VK_ACCESS_TRANSFER_WRITE_BIT;
		  imageMemoryBarrier.dstAccessMask = VK_ACCESS_SHADER_READ_BIT;
	  }

	  // Put barrier on top
	  VkPipelineStageFlags srcStageFlags = VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT;
	  VkPipelineStageFlags destStageFlags = VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT;

	  // Put barrier inside setup command buffer
	  vkCmdPipelineBarrier(
		  cmdbuffer,
		  srcStageFlags,
		  destStageFlags,
		  0,
		  0, nullptr,
		  0, nullptr,
		  1, &imageMemoryBarrier);
  }

  void VulkanCommon::PrepareDepthBuffer(VkCommandBuffer cmdBufferH) {
	  const VkFormat depth_format = VK_FORMAT_D16_UNORM;
	  VkImageCreateInfo image = {};
	  image.sType = VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO;
	  image.pNext = NULL;
	  image.imageType = VK_IMAGE_TYPE_2D;
	  image.format = depth_format;
	  image.extent = { Vulkan.SwapChain.Extent.width, Vulkan.SwapChain.Extent.height, 1 };
	  image.mipLevels = 1;
	  image.arrayLayers = 1;
	  image.samples = VK_SAMPLE_COUNT_1_BIT;
	  image.tiling = VK_IMAGE_TILING_OPTIMAL;
	  image.usage = VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT | VK_IMAGE_USAGE_TRANSFER_SRC_BIT;
	  image.flags = 0;
	  image.initialLayout = VK_IMAGE_LAYOUT_UNDEFINED;

	  VkMemoryAllocateInfo mem_alloc = {};
		  mem_alloc.sType = VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
		  mem_alloc.pNext = NULL;
		  mem_alloc.allocationSize = 0;
		  mem_alloc.memoryTypeIndex = 0;

	  VkImageViewCreateInfo depthStencilView = {};
		  depthStencilView.sType = VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO;
		  depthStencilView.pNext = NULL;
		  depthStencilView.viewType = VK_IMAGE_VIEW_TYPE_2D;
		  depthStencilView.format = depth_format;
		  depthStencilView.flags = 0;
		  depthStencilView.subresourceRange = {};
		  depthStencilView.subresourceRange.aspectMask = VK_IMAGE_ASPECT_DEPTH_BIT | VK_IMAGE_ASPECT_STENCIL_BIT;
		  depthStencilView.subresourceRange.baseMipLevel = 0;
		  depthStencilView.subresourceRange.levelCount = 1;
		  depthStencilView.subresourceRange.baseArrayLayer = 0;
		  depthStencilView.subresourceRange.layerCount = 1;

	  VkMemoryRequirements mem_reqs;
	  VkResult err;

	  Depth.Format = depth_format;

	  err = vkCreateImage(GetDevice(), &image, NULL, &Depth.Image);
	  assert(!err);

	  vkGetImageMemoryRequirements(GetDevice(), Depth.Image, &mem_reqs);

	  mem_alloc.allocationSize = mem_reqs.size;

	  err = vkAllocateMemory(GetDevice(), &mem_alloc, nullptr, &Depth.Mem);
	  assert(!err);

	  err = vkBindImageMemory(GetDevice(), Depth.Image, Depth.Mem, 0);
	  assert(!err);

	  VkImageSubresourceRange subResourceRange = {};
	  subResourceRange.aspectMask = VK_IMAGE_ASPECT_DEPTH_BIT | VK_IMAGE_ASPECT_STENCIL_BIT;
	  subResourceRange.baseMipLevel = 0;
	  subResourceRange.levelCount = 1;
	  subResourceRange.layerCount = 1;
	  setImageLayout(
		  cmdBufferH,
		  Depth.Image,
		  VK_IMAGE_ASPECT_DEPTH_BIT | VK_IMAGE_ASPECT_STENCIL_BIT,
		  VK_IMAGE_LAYOUT_UNDEFINED,
		  VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL,
		  subResourceRange);

	  depthStencilView.image = Depth.Image;
	  err = vkCreateImageView(GetDevice(), &depthStencilView, nullptr, &Depth.View);
	  assert(!err);
  }

  bool VulkanCommon::CreateSwapChain() {
    if( Vulkan.Device != VK_NULL_HANDLE ) {
      vkDeviceWaitIdle( Vulkan.Device );
    }

    for( size_t i = 0; i < Vulkan.SwapChain.Images.size(); ++i ) {
      if( Vulkan.SwapChain.Images[i].ImageView != VK_NULL_HANDLE ) {
        vkDestroyImageView( GetDevice(), Vulkan.SwapChain.Images[i].ImageView, nullptr );
        Vulkan.SwapChain.Images[i].ImageView = VK_NULL_HANDLE;
      }
    }
    Vulkan.SwapChain.Images.clear();

    VkSurfaceCapabilitiesKHR surface_capabilities;
    if( vkGetPhysicalDeviceSurfaceCapabilitiesKHR( Vulkan.PhysicalDevice, Vulkan.PresentationSurface, &surface_capabilities ) != VK_SUCCESS ) {
      std::cout << "Could not check presentation surface capabilities!" << std::endl;
      return false;
    }

    uint32_t formats_count;
    if( (vkGetPhysicalDeviceSurfaceFormatsKHR( Vulkan.PhysicalDevice, Vulkan.PresentationSurface, &formats_count, nullptr ) != VK_SUCCESS) ||
        (formats_count == 0) ) {
      std::cout << "Error occurred during presentation surface formats enumeration!" << std::endl;
      return false;
    }

    std::vector<VkSurfaceFormatKHR> surface_formats( formats_count );
    if( vkGetPhysicalDeviceSurfaceFormatsKHR( Vulkan.PhysicalDevice, Vulkan.PresentationSurface, &formats_count, &surface_formats[0] ) != VK_SUCCESS ) {
      std::cout << "Error occurred during presentation surface formats enumeration!" << std::endl;
      return false;
    }

    uint32_t present_modes_count;
    if( (vkGetPhysicalDeviceSurfacePresentModesKHR( Vulkan.PhysicalDevice, Vulkan.PresentationSurface, &present_modes_count, nullptr ) != VK_SUCCESS) ||
        (present_modes_count == 0) ) {
      std::cout << "Error occurred during presentation surface present modes enumeration!" << std::endl;
      return false;
    }

    std::vector<VkPresentModeKHR> present_modes( present_modes_count );
    if( vkGetPhysicalDeviceSurfacePresentModesKHR( Vulkan.PhysicalDevice, Vulkan.PresentationSurface, &present_modes_count, &present_modes[0] ) != VK_SUCCESS ) {
      std::cout << "Error occurred during presentation surface present modes enumeration!" << std::endl;
      return false;
    }

    uint32_t                      desired_number_of_images = GetSwapChainNumImages( surface_capabilities );
    VkSurfaceFormatKHR            desired_format = GetSwapChainFormat( surface_formats );
    VkExtent2D                    desired_extent = GetSwapChainExtent( surface_capabilities );
    VkImageUsageFlags             desired_usage = GetSwapChainUsageFlags( surface_capabilities );
    VkSurfaceTransformFlagBitsKHR desired_transform = GetSwapChainTransform( surface_capabilities );
    VkPresentModeKHR              desired_present_mode = GetSwapChainPresentMode( present_modes );
    VkSwapchainKHR                old_swap_chain = Vulkan.SwapChain.Handle;

    if( static_cast<int>(desired_usage) == -1 ) {
      return false;
    }
    if( static_cast<int>(desired_present_mode) == -1 ) {
      return false;
    }

    VkSwapchainCreateInfoKHR swap_chain_create_info = {
      VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR,  // VkStructureType                sType
      nullptr,                                      // const void                    *pNext
      0,                                            // VkSwapchainCreateFlagsKHR      flags
      Vulkan.PresentationSurface,                   // VkSurfaceKHR                   surface
      desired_number_of_images,                     // uint32_t                       minImageCount
      desired_format.format,                        // VkFormat                       imageFormat
      desired_format.colorSpace,                    // VkColorSpaceKHR                imageColorSpace
      desired_extent,                               // VkExtent2D                     imageExtent
      1,                                            // uint32_t                       imageArrayLayers
      desired_usage,                                // VkImageUsageFlags              imageUsage
      VK_SHARING_MODE_EXCLUSIVE,                    // VkSharingMode                  imageSharingMode
      0,                                            // uint32_t                       queueFamilyIndexCount
      nullptr,                                      // const uint32_t                *pQueueFamilyIndices
      desired_transform,                            // VkSurfaceTransformFlagBitsKHR  preTransform
      VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR,            // VkCompositeAlphaFlagBitsKHR    compositeAlpha
      desired_present_mode,                         // VkPresentModeKHR               presentMode
      VK_TRUE,                                      // VkBool32                       clipped
      old_swap_chain                                // VkSwapchainKHR                 oldSwapchain
    };

    if( vkCreateSwapchainKHR( Vulkan.Device, &swap_chain_create_info, nullptr, &Vulkan.SwapChain.Handle ) != VK_SUCCESS ) {
      std::cout << "Could not create swap chain!" << std::endl;
      return false;
    }
    if( old_swap_chain != VK_NULL_HANDLE ) {
      vkDestroySwapchainKHR( Vulkan.Device, old_swap_chain, nullptr );
    }

    Vulkan.SwapChain.Format = desired_format.format;

    uint32_t image_count = 0;
    if( (vkGetSwapchainImagesKHR( Vulkan.Device, Vulkan.SwapChain.Handle, &image_count, nullptr ) != VK_SUCCESS) ||
        (image_count == 0) ) {
      std::cout << "Could not get swap chain images!" << std::endl;
      return false;
    }
    Vulkan.SwapChain.Images.resize( image_count );

    std::vector<VkImage> images( image_count );
    if( vkGetSwapchainImagesKHR( Vulkan.Device, Vulkan.SwapChain.Handle, &image_count, &images[0] ) != VK_SUCCESS ) {
      std::cout << "Could not get swap chain images!" << std::endl;
      return false;
    }

    for( size_t i = 0; i < Vulkan.SwapChain.Images.size(); ++i ) {
      Vulkan.SwapChain.Images[i].Handle = images[i];
    }
    Vulkan.SwapChain.Extent = desired_extent;

    return CreateSwapChainImageViews();
  }

  bool VulkanCommon::CreateSwapChainImageViews() {
    for( size_t i = 0; i < Vulkan.SwapChain.Images.size(); ++i ) {
      VkImageViewCreateInfo image_view_create_info = {
        VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO,   // VkStructureType                sType
        nullptr,                                    // const void                    *pNext
        0,                                          // VkImageViewCreateFlags         flags
        Vulkan.SwapChain.Images[i].Handle,          // VkImage                        image
        VK_IMAGE_VIEW_TYPE_2D,                      // VkImageViewType                viewType
        GetSwapChain().Format,                      // VkFormat                       format
        {                                           // VkComponentMapping             components
          VK_COMPONENT_SWIZZLE_IDENTITY,              // VkComponentSwizzle             r
          VK_COMPONENT_SWIZZLE_IDENTITY,              // VkComponentSwizzle             g
          VK_COMPONENT_SWIZZLE_IDENTITY,              // VkComponentSwizzle             b
          VK_COMPONENT_SWIZZLE_IDENTITY               // VkComponentSwizzle             a
        },
        {                                           // VkImageSubresourceRange        subresourceRange
          VK_IMAGE_ASPECT_COLOR_BIT,                  // VkImageAspectFlags             aspectMask
          0,                                          // uint32_t                       baseMipLevel
          1,                                          // uint32_t                       levelCount
          0,                                          // uint32_t                       baseArrayLayer
          1                                           // uint32_t                       layerCount
        }
      };

      if( vkCreateImageView( GetDevice(), &image_view_create_info, nullptr, &Vulkan.SwapChain.Images[i].ImageView ) != VK_SUCCESS ) {
        std::cout << "Could not create image view for framebuffer!" << std::endl;
        return false;
      }
    }

    return true;
  }

  bool VulkanCommon::CheckExtensionAvailability( const char *extension_name, const std::vector<VkExtensionProperties> &available_extensions ) {
    for( size_t i = 0; i < available_extensions.size(); ++i ) {
      if( strcmp( available_extensions[i].extensionName, extension_name ) == 0 ) {
        return true;
      }
    }
    return false;
  }

  uint32_t VulkanCommon::GetSwapChainNumImages( VkSurfaceCapabilitiesKHR &surface_capabilities ) {
    // Set of images defined in a swap chain may not always be available for application to render to:
    // One may be displayed and one may wait in a queue to be presented
    // If application wants to use more images at the same time it must ask for more images
    uint32_t image_count = surface_capabilities.minImageCount + 1;
    if( (surface_capabilities.maxImageCount > 0) &&
        (image_count > surface_capabilities.maxImageCount) ) {
      image_count = surface_capabilities.maxImageCount;
    }
    return image_count;
  }

  VkSurfaceFormatKHR VulkanCommon::GetSwapChainFormat( std::vector<VkSurfaceFormatKHR> &surface_formats ) {
    // If the list contains only one entry with undefined format
    // it means that there are no preferred surface formats and any can be chosen
    if( (surface_formats.size() == 1) &&
        (surface_formats[0].format == VK_FORMAT_UNDEFINED) ) {
      return{ VK_FORMAT_R8G8B8A8_UNORM, VK_COLORSPACE_SRGB_NONLINEAR_KHR };
    }

    // Check if list contains most widely used R8 G8 B8 A8 format
    // with nonlinear color space
    for( VkSurfaceFormatKHR &surface_format : surface_formats ) {
      if( surface_format.format == VK_FORMAT_R8G8B8A8_UNORM ) {
        return surface_format;
      }
    }

    // Return the first format from the list
    return surface_formats[0];
  }

  VkExtent2D VulkanCommon::GetSwapChainExtent( VkSurfaceCapabilitiesKHR &surface_capabilities ) {
    // Special value of surface extent is width == height == -1
    // If this is so we define the size by ourselves but it must fit within defined confines
    if( surface_capabilities.currentExtent.width == -1 ) {
      VkExtent2D swap_chain_extent = { 640, 480 };
      if( swap_chain_extent.width < surface_capabilities.minImageExtent.width ) {
        swap_chain_extent.width = surface_capabilities.minImageExtent.width;
      }
      if( swap_chain_extent.height < surface_capabilities.minImageExtent.height ) {
        swap_chain_extent.height = surface_capabilities.minImageExtent.height;
      }
      if( swap_chain_extent.width > surface_capabilities.maxImageExtent.width ) {
        swap_chain_extent.width = surface_capabilities.maxImageExtent.width;
      }
      if( swap_chain_extent.height > surface_capabilities.maxImageExtent.height ) {
        swap_chain_extent.height = surface_capabilities.maxImageExtent.height;
      }
      return swap_chain_extent;
    }

    // Most of the cases we define size of the swap_chain images equal to current window's size
    return surface_capabilities.currentExtent;
  }

  VkImageUsageFlags VulkanCommon::GetSwapChainUsageFlags( VkSurfaceCapabilitiesKHR &surface_capabilities ) {
    // Color attachment flag must always be supported
    // We can define other usage flags but we always need to check if they are supported
    if( surface_capabilities.supportedUsageFlags & VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT ) {
      return VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT;
    }
    std::cout << "VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT image usage is not supported by the swap chain!" << std::endl
      << "Supported swap chain's image usages include:" << std::endl
      << (surface_capabilities.supportedUsageFlags & VK_IMAGE_USAGE_TRANSFER_SRC_BIT              ? "    VK_IMAGE_USAGE_TRANSFER_SRC\n" : "")
      << (surface_capabilities.supportedUsageFlags & VK_IMAGE_USAGE_TRANSFER_DST_BIT              ? "    VK_IMAGE_USAGE_TRANSFER_DST\n" : "")
      << (surface_capabilities.supportedUsageFlags & VK_IMAGE_USAGE_SAMPLED_BIT                   ? "    VK_IMAGE_USAGE_SAMPLED\n" : "")
      << (surface_capabilities.supportedUsageFlags & VK_IMAGE_USAGE_STORAGE_BIT                   ? "    VK_IMAGE_USAGE_STORAGE\n" : "")
      << (surface_capabilities.supportedUsageFlags & VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT          ? "    VK_IMAGE_USAGE_COLOR_ATTACHMENT\n" : "")
      << (surface_capabilities.supportedUsageFlags & VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT  ? "    VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT\n" : "")
      << (surface_capabilities.supportedUsageFlags & VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT      ? "    VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT\n" : "")
      << (surface_capabilities.supportedUsageFlags & VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT          ? "    VK_IMAGE_USAGE_INPUT_ATTACHMENT" : "")
      << std::endl;
    return static_cast<VkImageUsageFlags>(-1);
  }

  VkSurfaceTransformFlagBitsKHR VulkanCommon::GetSwapChainTransform( VkSurfaceCapabilitiesKHR &surface_capabilities ) {
    // Sometimes images must be transformed before they are presented (i.e. due to device's orienation
    // being other than default orientation)
    // If the specified transform is other than current transform, presentation engine will transform image
    // during presentation operation; this operation may hit performance on some platforms
    // Here we don't want any transformations to occur so if the identity transform is supported use it
    // otherwise just use the same transform as current transform
    if( surface_capabilities.supportedTransforms & VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR ) {
      return VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR;
    } else {
      return surface_capabilities.currentTransform;
    }
  }

  VkPresentModeKHR VulkanCommon::GetSwapChainPresentMode( std::vector<VkPresentModeKHR> &present_modes ) {
    // FIFO present mode is always available
    // MAILBOX is the lowest latency V-Sync enabled mode (something like triple-buffering) so use it if available
    for( VkPresentModeKHR &present_mode : present_modes ) {
      if( present_mode == VK_PRESENT_MODE_MAILBOX_KHR ) {
        return present_mode;
      }
    }
    for( VkPresentModeKHR &present_mode : present_modes ) {
      if( present_mode == VK_PRESENT_MODE_FIFO_KHR ) {
        return present_mode;
      }
    }
    std::cout << "FIFO present mode is not supported by the swap chain!" << std::endl;
    return static_cast<VkPresentModeKHR>(-1);
  }

  VulkanCommon::~VulkanCommon() {
    if( Vulkan.Device != VK_NULL_HANDLE ) {
      vkDeviceWaitIdle( Vulkan.Device );

      for( size_t i = 0; i < Vulkan.SwapChain.Images.size(); ++i ) {
        if( Vulkan.SwapChain.Images[i].ImageView != VK_NULL_HANDLE ) {
          vkDestroyImageView( GetDevice(), Vulkan.SwapChain.Images[i].ImageView, nullptr );
        }
      }

      if( Vulkan.SwapChain.Handle != VK_NULL_HANDLE ) {
        vkDestroySwapchainKHR( Vulkan.Device, Vulkan.SwapChain.Handle, nullptr );
      }
      vkDestroyDevice( Vulkan.Device, nullptr );
    }

    if( Vulkan.PresentationSurface != VK_NULL_HANDLE ) {
      vkDestroySurfaceKHR( Vulkan.Instance, Vulkan.PresentationSurface, nullptr );
    }

    if( Vulkan.Instance != VK_NULL_HANDLE ) {
      vkDestroyInstance( Vulkan.Instance, nullptr );
    }

    if( VulkanLibrary ) {
#if defined(VK_USE_PLATFORM_WIN32_KHR)
      FreeLibrary( VulkanLibrary );
#elif defined(VK_USE_PLATFORM_XCB_KHR) || defined(VK_USE_PLATFORM_XLIB_KHR)
      dlclose( VulkanLibrary );
#endif
    }
  }

} // namespace ApiWithoutSecrets